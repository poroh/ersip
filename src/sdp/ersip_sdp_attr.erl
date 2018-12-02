%%
%% Copyright (c) 2018 Dmitry Poroh
%% All rights reserved.
%% Distributed under the terms of the MIT License. See the LICENSE file.
%%
%% SDP attributes
%%

-module(ersip_sdp_attr).

-export([parse/1]).

-export_type([attr_list/0]).

%%%===================================================================
%%% Types
%%%===================================================================

-type attr_list()    :: [attr()].
-type parse_result() :: ersip_parser_aux:parse_result(attr_list()).
-type attr()         :: attr_name()
                      | {attr_name(), attr_value()}.
-type attr_name()    :: binary().
-type attr_value()   :: binary().

%%%===================================================================
%%% API
%%%===================================================================

-spec parse(binary()) -> parse_result().
parse(Bin) ->
    do_parse_attrs(Bin, []).

%%%===================================================================
%%% Internal Implementation
%%%===================================================================

-define(crlf, "\r\n").

-spec do_parse_attrs(binary(), attr_list()) -> parse_result().
do_parse_attrs(<<"a=", Rest/binary>>, Acc) ->
    case binary:split(Rest, <<?crlf>>) of
        [_] ->
            {error, {invalid_attr, {no_crlf, Rest}}};
        [AttrLine, Rest1] ->
            {AttrName, _} = Pair =
                case binary:split(AttrLine, <<":">>) of
                    [N, V] -> {N, V};
                    [N]    -> {N, novalue}
                end,
            case ersip_sdp_aux:check_token(AttrName) of
                false ->
                    {error, {invalid_attr, AttrName}};
                true ->
                    do_parse_attrs(Rest1, [make_attr(Pair) | Acc])
            end
    end;
do_parse_attrs(Rest, Acc) ->
    {ok, lists:reverse(Acc), Rest}.

-spec make_attr({attr_name(), attr_value() | novalue}) -> attr().
make_attr({Name, novalue}) ->
    Name;
make_attr({_, _} = Attr) ->
    Attr.

