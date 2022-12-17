%%%
%%% Copyright (c) 2018, 2020 Dmitry Poroh
%%% All rights reserved.
%%% Distributed under the terms of the MIT License. See the LICENSE file.
%%%
%%% SIP Date header
%%%

-module(ersip_hdr_date).

-export([make/1,
         make/2,
         date/1,
         time/1,
         now/0,
         is_valid/1,
         parse/1,
         build/2,
         assemble/1,
         assemble_bin/1,
         raw/1
        ]).
-export_type([datetime/0, date/0, time/0, raw/0]).

%%===================================================================
%% Types
%%===================================================================

-type datetime() :: {date, calendar:datetime()}.
-type date() :: calendar:date().
-type time() :: calendar:time().
-type parse_result() :: {ok, datetime()} | {error, parse_error()}.
-type parse_error() :: {invalid_date, bad_timezone}
                     | {invalid_date, bad_datetime}
                     | {invalid_date, wrong_weekday}
                     | {invalid_date, {incorrect_date, triplet()}}
                     | {invalid_date, {incorrect_time, triplet()}}
                     | {invalid_date, no_datetime}.
-type lex_token() :: colon | comma | plus | minus | {int, non_neg_integer()} |
                     {month, 1..12} | {wkday, 1..7} | {timezone, timezone()} | char().
-type timezone() :: gmt | utc | ut | est | edt | mst | mdt | pst | pdt.
-type triplet() :: {non_neg_integer(), non_neg_integer(), non_neg_integer()}.
-type raw() :: {date(), time()}.

%%===================================================================
%% API
%%===================================================================

%% @doc Make Date header from binary, raw representation of Date or
%% from raw SIP header. If syntax is invalid then this function raises
%% error.
-spec make(binary() | ersip_hdr:header() | calendar:datetime()) -> datetime().
make(Bin) when is_binary(Bin) ->
    case parse_datetime(Bin) of
        {ok, DateTime} -> DateTime;
        {error, Reason} -> error(Reason)
    end;
make({Date, Time}) ->
    case is_correct_date(Date) of
        true -> ok;
        false -> error({invalid_date, {incorrect_date, Date}})
    end,
    case is_correct_time(Time) of
        true -> ok;
        false -> error({invalid_date, {incorrect_time, Time}})
    end,
    {date, {Date, Time}};
make(Header) ->
    case parse(Header) of
        {ok, DateTime} -> DateTime;
        {error, Reason} -> error(Reason)
    end.

%% @doc Make date header from date and time.
-spec make(date(), time()) -> datetime().
make(Date, Time) ->
    {date, {Date, Time}}.

%% @doc Date header for current moment of time.
-spec now() -> datetime().
now() ->
    {date, calendar:universal_time()}.


%% @doc Extract date from Date header (GMT).
-spec date(datetime()) -> date().
date({date, {Date, _}}) ->
    Date.

%% @doc Extract time from Time header (GMT).
-spec time(datetime()) -> time().
time({date, {_, Time}}) ->
    Time.

%% @doc Check that Erlang term is valid Date header.
-spec is_valid(term()) -> boolean().
is_valid({date, {{_, _ , _}, {_, _, _}}} = DT) ->
    is_correct_time(time(DT)) andalso is_correct_date(date(DT));
is_valid(_) ->
    false.

%% @doc Parse Date header from binary or from raw SIP header.
-spec parse(binary() | ersip_hdr:header()) -> parse_result().
parse(Bin) when is_binary(Bin) ->
    parse_datetime(Bin);
parse(Header) ->
    case ersip_hdr:raw_values(Header) of
        [] ->
            {error, {invalid_date, no_datetime}};
        [DateIOList]  ->
            parse_datetime(iolist_to_binary(DateIOList))
    end.

%% @doc Build SIP header.
-spec build(HeaderName :: binary(), datetime()) -> ersip_hdr:header().
build(HdrName, DateTime) ->
    Hdr = ersip_hdr:new(HdrName),
    ersip_hdr:add_value(assemble(DateTime), Hdr).

%% @doc Serialize Date header to iolist.
-spec assemble(datetime()) -> iolist().
assemble({date, _} = DT) ->
    [str_impl(DT)].

%% @doc Serialize Date header to binary.
-spec assemble_bin(datetime()) -> binary().
assemble_bin({date, _} = DT) ->
    iolist_to_binary(assemble(DT)).

%% @doc Raw representation of Date header..
-spec raw(datetime()) -> raw().
raw({date, DT}) ->
    DT.

%%===================================================================
%% Internal implementation
%%===================================================================

-define(RANGE(X, From, To), (X >= From andalso X=< To)).
-define(SPACE, 32).

-spec parse_datetime(binary()) -> parse_result().
parse_datetime(Str) ->
    S = ersip_bin:to_lower(Str),
    L = lex(S,[]),
    case match(L) of
        {ok, _} = Ok -> Ok;
        {error, _} = Error -> Error;
        nomatch -> {error, {invalid_date, Str}}
    end.

%% RFC1123
%% Fields in the reverse order
-spec match([lex_token()]) -> parse_result() | nomatch.
match([{timezone, gmt},
       {int, S}, colon, {int, Min}, colon, {int, H},
       {int, Y}, {month, M},  {int, D},
       comma, {wkday, W}]) ->
    correct_datetime({{Y, M, D}, {H, Min, S}}, W);
match([{timezone, TZ}   | _]) -> {error, {invalid_date, {bad_timezone, TZ}}};
match([{int, TZ}, plus  | _]) -> {error, {invalid_date, {bad_timezone, TZ}}};
match([{int, TZ}, minus | _]) -> {error, {invalid_date, {bad_timezone, TZ}}};
match(_A)                     -> nomatch.

-spec lex(binary(), [lex_token()]) -> [lex_token()].
lex(<<>>, Acc) -> Acc;
lex(<<?SPACE , Rest/binary>>, Acc)       -> lex(Rest, Acc);
lex(<< D/utf8, Rest/binary>>, Acc) when ?RANGE(D, $0, $9) ->
    {Num, R1} = fetch_digits(Rest, D - $0),
    lex(R1, [{int, Num}| Acc]);
lex(Bin, Acc) ->
    {Token, Rest} = lex_match(Bin),
    lex(Rest, [Token | Acc]).

-spec lex_match(binary()) -> {lex_token(), binary()}.
lex_match(<<$:, R/binary>>) -> {colon, R};
lex_match(<<$,, R/binary>>) -> {comma, R};
lex_match(<<$+, R/binary>>) -> {plus,  R};
lex_match(<<$-, R/binary>>) -> {minus, R};
lex_match(<<"jan", R/binary>>) -> {{month, 1}, R};
lex_match(<<"feb", R/binary>>) -> {{month, 2}, R};
lex_match(<<"mar", R/binary>>) -> {{month, 3}, R};
lex_match(<<"apr", R/binary>>) -> {{month, 4}, R};
lex_match(<<"may", R/binary>>) -> {{month, 5}, R};
lex_match(<<"jun", R/binary>>) -> {{month, 6}, R};
lex_match(<<"jul", R/binary>>) -> {{month, 7}, R};
lex_match(<<"aug", R/binary>>) -> {{month, 8}, R};
lex_match(<<"sep", R/binary>>) -> {{month, 9}, R};
lex_match(<<"oct", R/binary>>) -> {{month, 10}, R};
lex_match(<<"nov", R/binary>>) -> {{month, 11}, R};
lex_match(<<"dec", R/binary>>) -> {{month, 12}, R};
lex_match(<<"mon", R/binary>>) -> {{wkday, 1}, R};
lex_match(<<"tue", R/binary>>) -> {{wkday, 2}, R};
lex_match(<<"wed", R/binary>>) -> {{wkday, 3}, R};
lex_match(<<"thu", R/binary>>) -> {{wkday, 4}, R};
lex_match(<<"fri", R/binary>>) -> {{wkday, 5}, R};
lex_match(<<"sat", R/binary>>) -> {{wkday, 6}, R};
lex_match(<<"sun", R/binary>>) -> {{wkday, 7}, R};
lex_match(<<"gmt", R/binary>>) -> {{timezone, gmt}, R}; % according to rfc can be GMT only
lex_match(<<"utc", R/binary>>) -> {{timezone, utc}, R};
lex_match(<<"ut",  R/binary>>) -> {{timezone, ut }, R};
lex_match(<<"est", R/binary>>) -> {{timezone, est}, R};
lex_match(<<"edt", R/binary>>) -> {{timezone, edt}, R};
lex_match(<<"mst", R/binary>>) -> {{timezone, mst}, R};
lex_match(<<"mdt", R/binary>>) -> {{timezone, mdt}, R};
lex_match(<<"pst", R/binary>>) -> {{timezone, pst}, R};
lex_match(<<"pdt", R/binary>>) -> {{timezone, pdt}, R};
lex_match(<<D/utf8, R/binary>>) -> {D, R}.

-spec fetch_digits(binary(), non_neg_integer()) -> {non_neg_integer(), binary()}.
fetch_digits(<<D/utf8, R/binary>>, Acc) when ?RANGE(D, $0, $9) ->
    fetch_digits(R, (D - $0) + Acc*10);
fetch_digits(R, Acc) ->
    {Acc, R}.

-spec correct_datetime({triplet(), triplet()}, non_neg_integer()) -> parse_result().
correct_datetime({Date, Time} = DateTime, Weekday) ->
    try
        {t, true} = {t, is_correct_time(Time)},
        {d, true} = {d, is_correct_date(Date)},
        {w, true} = {w, Weekday == calendar:day_of_the_week(Date)},
        {ok, {date, DateTime}}
    catch
        _: {badmatch, {w, _}} -> {error, {invalid_date, wrong_weekday}};
        _: {badmatch, {d, _}} -> {error, {invalid_date, {incorrect_date, Date}}};
        _: {badmatch, {t, _}} -> {error, {invalid_date, {incorrect_time, Time}}}
    end.

-spec is_correct_time(term()) -> boolean().
is_correct_time({H, M, S}) when is_integer(H), is_integer(M), is_integer(S) ->
    ?RANGE(S, 0, 59) andalso ?RANGE(H, 0, 23) andalso ?RANGE(M, 0, 59);
is_correct_time(_) ->
    false.

-spec is_correct_date(term()) -> boolean().
is_correct_date({Y, M, D} = Date) when is_integer(Y), is_integer(M), is_integer(D) ->
    calendar:valid_date(Date);
is_correct_date(_) ->
    false.

-spec str_impl(datetime()) -> binary().
str_impl({date, { {Y, M, D} = Date, {H, Min, S} }}) ->
    Month =
        case M of
            1  -> "Jan";
            2  -> "Feb";
            3  -> "Mar";
            4  -> "Apr";
            5  -> "May";
            6  -> "Jun";
            7  -> "Jul";
            8  -> "Aug";
            9  -> "Sep";
            10 -> "Oct";
            11 -> "Nov";
            12 -> "Dec"
        end,
    Weekday =
        case calendar:day_of_the_week(Date) of
            1 -> "Mon";
            2 -> "Tue";
            3 -> "Wed";
            4 -> "Thu";
            5 -> "Fri";
            6 -> "Sat";
            7 -> "Sun"
        end,
    iolist_to_binary(io_lib:format("~s, ~b ~s ~b ~2..0b:~2..0b:~2..0b GMT",[Weekday, D, Month, Y, H, Min, S])).
