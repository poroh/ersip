%%
%% Copyright (c) 2018 Dmitry Poroh
%% All rights reserved.
%% Distributed under the terms of the MIT License. See the LICENSE file.
%%
%% SDP bandwidth
%%

-module(ersip_sdp_bandwidth).

-export([tias/1,
         ct/1,
         as/1,
         experimental/2,
         parse/1,
         assemble/1
        ]).

-export_type([bandwidth/0]).

%%%===================================================================
%%% Types
%%%===================================================================

-type bandwidth() :: {bandwidth, bw_list()}.
-type bw_list()  :: [bw_item()].
-type bw_item()  :: {bw_type(), bw_value()}.
-type bw_type()  :: ct | as | tias | {bw_type, binary()}.
-type bw_value() :: non_neg_integer().

-type parse_result() :: ersip_parser_aux:parse_result(bandwidth()).

%%%===================================================================
%%% API
%%%===================================================================

-spec tias(bandwidth()) -> non_neg_integer() | undefined.
tias({bandwidth, BWList}) ->
    proplists:get_value(tias, BWList, undefined).

-spec as(bandwidth()) -> non_neg_integer() | undefined.
as({bandwidth, BWList}) ->
    proplists:get_value(as, BWList, undefined).

-spec ct(bandwidth()) -> non_neg_integer() | undefined.
ct({bandwidth, BWList}) ->
    proplists:get_value(ct, BWList, undefined).

-spec experimental(binary(), bandwidth()) -> non_neg_integer() | undefined.
experimental(Name, {bandwidth, BWList}) ->
    LName = ersip_bin:to_lower(Name),
    Vals = [Val || {{bw_type, N}, Val} <- BWList,
                   ersip_bin:to_lower(N) == LName],
    case Vals of
        [] ->
            undefined;
        [V|_] -> V
    end.

-spec parse(binary()) -> parse_result().
parse(Bin) ->
    do_bw_parse(Bin, {bandwidth, []}).

-spec assemble(bandwidth()) -> iolist().
assemble({bandwidth, BWList}) ->
    [[<<"b=">>, bw_type_to_binary(BWType), <<":">>, integer_to_binary(BWValue), <<"\r\n">>]
     || {BWType, BWValue} <- BWList].

%%%===================================================================
%%% Internal Implementation
%%%===================================================================
-define(crlf, "\r\n").

%% bandwidth-fields =    *(%x62 "=" bwtype ":" bandwidth CRLF)
-spec do_bw_parse(binary(), bandwidth()) -> ersip_parser_aux:parse_result(bandwidth()).
do_bw_parse(<<"b=", Rest/binary>>, {bandwidth, Acc}) ->
    case binary:split(Rest, <<?crlf>>) of
        [_] ->
            {error, {invalid_bandwidth, {no_crlf, Rest}}};
        [Band, Rest1] ->
            case parse_bw(Band) of
                {ok, BWItem} ->
                    do_bw_parse(Rest1, {bandwidth, [BWItem | Acc]});
                {error, _} = Error ->
                    Error
            end
    end;
do_bw_parse(Bin, {bandwidth, Acc}) ->
    {ok, {bandwidth, lists:reverse(Acc)}, Bin}.

-spec parse_bw(binary()) -> {ok, bw_item()} | {error, term()}.
parse_bw(Item) ->
    case binary:split(Item, <<":">>) of
        [_] ->
            {error, {invalid_bandwidth, Item}};
        [BWTypeBin, BWValueBin] ->
            case ersip_sdp_aux:check_token(BWTypeBin) of
                false ->
                    {error, {invalid_bandwidth_type, BWTypeBin}};
                true ->
                    BWType = convert_bw_type(BWTypeBin),
                    case ersip_parser_aux:parse_non_neg_int(BWValueBin) of
                        {ok, BWValue, <<>>} ->
                            {ok, {BWType, BWValue}};
                        {ok, _, _} ->
                            {error, {invalid_bandwidth_value, BWValueBin}};
                        {error, Reason} ->
                            {error, {invalid_bandwidth_value, Reason}}
                    end
            end
    end.

-spec convert_bw_type(binary()) -> bw_type().
convert_bw_type(BWType) ->
    case ersip_bin:to_lower(BWType) of
        <<"ct">> -> ct;
        <<"as">> -> as;
        <<"tias">> -> tias; %% RFC 3890
        _ -> {bw_type, BWType}
    end.

-spec bw_type_to_binary(bw_type()) -> binary().
bw_type_to_binary(ct) ->
    <<"CT">>;
bw_type_to_binary(as) ->
    <<"AS">>;
bw_type_to_binary(tias) ->
    <<"TIAS">>;
bw_type_to_binary({bw_type, T}) ->
    T.


