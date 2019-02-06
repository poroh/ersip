%%
%% Copyright (c) 2018 Dmitry Poroh
%% All rights reserved.
%% Distributed under the terms of the MIT License. See the LICENSE file.
%%
%% SDP time
%%

-module(ersip_sdp_time).

-export([start/1,
         stop/1,
         parse/1,
         assemble/1
        ]).

-export_type([timings/0]).

%%%===================================================================
%%% Types
%%%===================================================================

-record(timings, {items :: nonempty_list(sess_item()),
                  zone  :: binary() | undefined}).
-type timings() :: #timings{}.

-record(sess_item, {start    :: ntp_timestamp(),
                    stop     :: ntp_timestamp(),
                    repeat   :: [binary()]}).
-type sess_item() :: #sess_item{}.
-type ntp_timestamp() :: non_neg_integer().
-type parse_result(X) :: ersip_parser_aux:parse_result(X).

%%%===================================================================
%%% API
%%%===================================================================

-spec start(timings()) -> ntp_timestamp().
start(#timings{items = [#sess_item{start = S}]}) ->
    S.

-spec stop(timings()) -> ntp_timestamp().
stop(#timings{items = [#sess_item{stop = S}]}) ->
    S.

%% time-fields = 1*( %x74 "=" start-time SP stop-time
%%                *(CRLF repeat-fields) CRLF)
%%                 [zone-adjustments CRLF]
-spec parse(binary()) -> parse_result(timings()).
parse(Bin) ->
    Parsers = [fun parse_items/1,
               fun parse_zone/1
              ],
    case ersip_parser_aux:parse_all(Bin, Parsers) of
        {ok, [Items, Zone], Rest} ->
            Timeings =
                #timings{items = Items,
                         zone  = Zone},
            {ok, Timeings, Rest};
        {error, _} = Error ->
            Error
    end.

-spec assemble(timings()) -> iolist().
assemble(#timings{items = Items, zone  = Zone}) ->
    [assemble_items(Items), assemble_zone(Zone)].

%%%===================================================================
%%% Internal implementation
%%%===================================================================

-define(crlf, "\r\n").

-spec parse_items(binary()) -> parse_result(nonempty_list(sess_item())).
parse_items(Bin) ->
    do_parse_items(Bin, []).

-spec assemble_items([sess_item()]) -> iolist().
assemble_items(Items) ->
    [[<<"t=">>, integer_to_binary(Start), <<" ">>, integer_to_binary(Stop), ?crlf,
      assemble_repeats(Repeat)]
     || #sess_item{start = Start, stop = Stop, repeat = Repeat} <- Items].

-spec assemble_repeats([binary()]) -> iolist().
assemble_repeats(Repeats) ->
    [[<<"r=">>, Repeat, ?crlf] || Repeat <- Repeats].

%% %x7a "=" time SP ["-"] typed-time *(SP time SP ["-"] typed-time) CRLF
-spec parse_zone(binary()) -> parse_result(binary() | undefined).
parse_zone(<<"z=", Rest/binary>>) ->
    case binary:split(Rest, <<?crlf>>) of
        [_] ->
            {error, {invalid_zone, {no_crlf, Rest}}};
        [V, Rest1] ->
            {ok, V, Rest1}
    end;
parse_zone(Other) ->
    {ok, undefined, Other}.

-spec assemble_zone(binary() | undefined) -> iolist().
assemble_zone(undefined) ->
    [];
assemble_zone(Zone) ->
    [<<"z=">>, Zone, ?crlf].

%% 1*( %x74 "=" start-time SP stop-time
%%                *(CRLF repeat-fields) CRLF)
-spec do_parse_items(binary(), [sess_item()]) -> parse_result(nonempty_list(sess_item())).
do_parse_items(<<"t=", _/binary>> = Bin, Acc) ->
    Parsers = [fun parse_time/1,
               fun parse_repeats/1
              ],
    case ersip_parser_aux:parse_all(Bin, Parsers) of
        {ok, [{StartTime, StopTime}, Repeat], Rest1} ->
           SessItem = #sess_item{start = StartTime,
                                 stop  = StopTime,
                                 repeat = Repeat},
            do_parse_items(Rest1, [SessItem | Acc]);
        {error, _} = Error ->
            Error
    end;
do_parse_items(Bin, Acc) ->
    case Acc of
        [] ->
            {error, no_time_specified};
        Acc ->
            {ok, lists:reverse(Acc), Bin}
    end.

-spec parse_time(binary()) -> parse_result({ntp_timestamp(), ntp_timestamp()}).
parse_time(<<"t=", Rest/binary>>) ->
    case binary:split(Rest, <<?crlf>>) of
        [_] ->
            {error, {invalid_time, {no_crlf, Rest}}};
        [V, Rest1] ->
            Parsers = [fun ersip_parser_aux:parse_non_neg_int/1,
                       fun ersip_parser_aux:parse_lws/1,
                       fun ersip_parser_aux:parse_non_neg_int/1
                      ],
            case ersip_parser_aux:parse_all(V, Parsers) of
                {ok, [StartTime, _, StopTime], <<>>} ->
                    {ok, {StartTime, StopTime}, Rest1};
                {ok, _, _} ->
                    {error, {invalid_time, V}};
                {error, Reason} ->
                    {error, {invalid_time, Reason}}
            end
    end.


-spec parse_repeats(binary()) -> parse_result([binary()]).
parse_repeats(Bin) ->
    do_parse_repeats(Bin, []).

-spec do_parse_repeats(binary(), [binary()]) -> parse_result([binary()]).
do_parse_repeats(<<"r=", Rest/binary>>, Acc) ->
    case binary:split(Rest, <<?crlf>>) of
        [_] ->
            {error, {invalid_time, {no_crlf, Rest}}};
        [V, Rest1] ->
            do_parse_repeats(Rest1, [V | Acc])
    end;
do_parse_repeats(Rest, Acc) ->
    {ok, lists:reverse(Acc), Rest}.
