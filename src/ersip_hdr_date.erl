%%
%% Copyright (c) 2018 Dmitry Poroh
%% All rights reserved.
%% Distributed under the terms of the MIT License. See the LICENSE file.
%%
%% SIP Date header
%%

-module(ersip_hdr_date).

-export([make/1,
         make/2,
         make_key/1,
         date/1,
         time/1,
         now/0,
         is_valid/1,
         parse/1,
         build/2,
         assemble/1
        ]).
-export_type([datetime/0, date/0, time/0]).

%%%===================================================================
%%% Types
%%%===================================================================

-type datetime() :: {date, calendar:datetime()}.
-type date() :: calendar:date().
-type time() :: calendar:time().

%%%===================================================================
%%% API
%%%===================================================================

-spec make(ersip_hdr:header()) -> datetime().
make(Header) ->
    case parse(Header) of
        {ok, DateTime} ->
            DateTime;
        Error ->
            error(Error)
    end.

-spec now() -> datetime().
now() -> {date, calendar:universal_time()}.

-spec make(date(), time()) -> datetime().
make(Date, Time) ->
    {date, {Date, Time}}.

-spec make_key(datetime()) -> datetime().
make_key(DT) ->
    DT.

-spec date(datetime()) -> date().
date({date, {Date, _}}) -> Date.

-spec time(datetime()) -> time().
time({date, {_, Time}}) -> Time.

-spec is_valid(datetime()) -> boolean().
is_valid({date, _} = DT) ->
    is_correct_time(time(DT)) andalso calendar:valid_date(date(DT));
is_valid(_) ->
    false.

-spec parse(ersip_hdr:header()) -> Result when
      Result :: {ok, datetime()}
              | {error, Error},
      Error :: bad_timezone
             | bad_datetime
             | wrong_weekday
             | incorrect_date
             | incorrect_time
             | no_datetime.
parse(Header) ->
    case ersip_hdr:raw_values(Header) of
        [] ->
            {error, no_datetime};
        [DateIOList]  ->
            parse_datetime(iolist_to_binary(DateIOList))
    end.

-spec build(HeaderName :: binary(), datetime()) -> ersip_hdr:header().
build(HdrName, DateTime) ->
    Hdr = ersip_hdr:new(HdrName),
    ersip_hdr:add_value(assemble(DateTime), Hdr).

-spec assemble(datetime()) -> [binary(), ... ].
assemble(DateTime) ->
    [str_impl(DateTime)].

%%%===================================================================
%%% Internal implementation
%%%===================================================================
-define(RANGE(X, From, To), (X >= From andalso X=< To)).
-define(PREFIX(S, Class), lex(<<S, Rest/binary>>, Acc)  -> lex(Rest, [Class | Acc])).
-define(SPACE, 32).

parse_datetime(Str) ->
    S = ersip_bin:to_lower(Str),
    L = lex(S,[]),
    match(L).

%% RFC1123
%% Fields in the reverse order
 match([{timezone, gmt},
        {int, S}, colon, {int, Min}, colon, {int, H},
        {int, Y}, {month, M},  {int, D},
        comma, {wkday, W}]) ->
    correct_datetime({{Y, M, D}, {H, Min, S}}, W);
match([{timezone, _TZ}   | _]) -> {error, bad_timezone};
match([{int, _TZ}, plus  | _]) -> {error, bad_timezone};
match([{int, _TZ}, minus | _]) -> {error, bad_timezone};
match(_A)                      -> {error, bad_datetime}.


lex(<<>>, Acc) -> Acc;
lex(<<?SPACE , Rest/binary>>, Acc)       -> lex(Rest, Acc);
lex(<< D/utf8, Rest/binary>>, Acc) when ?RANGE(D, $0, $9) ->
    {Num, R1} = fetch_digits(Rest, D - $0),
    lex(R1, [{int, Num}| Acc]);
?PREFIX($:, colon);
?PREFIX($,, comma);
?PREFIX($+, plus);
?PREFIX($-, minus);
?PREFIX("jan", {month, 1});
?PREFIX("feb", {month, 2});
?PREFIX("mar", {month, 3});
?PREFIX("apr", {month, 4});
?PREFIX("may", {month, 5});
?PREFIX("jun", {month, 6});
?PREFIX("jul", {month, 7});
?PREFIX("aug", {month, 8});
?PREFIX("sep", {month, 9});
?PREFIX("oct", {month, 10});
?PREFIX("nov", {month, 11});
?PREFIX("dec", {month, 12});
?PREFIX("mon", {wkday, 1});
?PREFIX("tue", {wkday, 2});
?PREFIX("wed", {wkday, 3});
?PREFIX("thu", {wkday, 4});
?PREFIX("fri", {wkday, 5});
?PREFIX("sat", {wkday, 6});
?PREFIX("sun", {wkday, 7});
?PREFIX("gmt", {timezone, gmt}); % according to rfc can be GMT only
?PREFIX("utc", {timezone, utc});
?PREFIX("ut",  {timezone, ut });
?PREFIX("est", {timezone, est});
?PREFIX("edt", {timezone, edt});
?PREFIX("mst", {timezone, mst});
?PREFIX("mdt", {timezone, mdt});
?PREFIX("pst", {timezone, pst});
?PREFIX("pdt", {timezone, pdt});
lex(<<D/utf8, Rest/binary>>, Acc)     -> lex(Rest, [D | Acc]).

fetch_digits(<<D/utf8, R/binary>>, Acc) when ?RANGE(D, $0, $9) ->
    fetch_digits(R, (D - $0) + Acc*10);
fetch_digits(R, Acc) ->
    {Acc, R}.

correct_time(HMS)              -> {t, is_correct_time(HMS)}.
correct_date(Date)             -> {d, calendar:valid_date(Date)}.
correct_weekday(Date, Weekday) -> {w, Weekday == calendar:day_of_the_week(Date)}.

correct_datetime({Date, Time} = DateTime, Weekday) ->
    try
        {t, true} = correct_time(Time),
        {d, true} = correct_date(Date),
        {w, true} = correct_weekday(Date, Weekday),
        {ok, {date, DateTime}}
    catch
        _: {badmatch, {w, _}} -> {error, wrong_weekday};
        _: {badmatch, {d, _}} -> {error, incorrect_date};
        _: {badmatch, {t, _}} -> {error, incorrect_time}
    end.

str_impl({date, DateTime}) ->
    list_to_binary(httpd_util:rfc1123_date(calendar:universal_time_to_local_time(DateTime))).

is_correct_time({H, M, S}) -> ?RANGE(S, 0, 59) andalso ?RANGE(H, 0, 23) andalso ?RANGE(M, 0, 59).
