%%
%% Copyright (c) 2018 Dmitry Poroh
%% All rights reserved.
%% Distributed under the terms of the MIT License. See the LICENSE file.
%%
%% SDP attributes
%%

-module(ersip_sdp_attr).

-export([parse/1,
    parse_attr/1,
    assemble/1
]).

-export_type([attr_list/0, attr/0]).

%%%===================================================================
%%% Types
%%%===================================================================

-type attr_list()    :: [attr()].
-type parse_result() :: ersip_parser_aux:parse_result(attr_list()).
-type parse_result(T) :: ersip_parser_aux:parse_result(T).
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

-spec parse_attr(binary()) -> parse_result(attr()).
parse_attr(Bin) ->
    do_parse_attr(Bin).

-spec assemble(attr_list()) -> iolist().
assemble(AttrList) ->
    [[<<"a=">>,
      case Attr of
          {AttrName, AttrValue} ->
              [AttrName, <<":">>, AttrValue];
          AttrName ->
              AttrName
      end,
      <<"\r\n">>]
     || Attr <- AttrList].

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
            case do_parse_attr(AttrLine) of
                {error,_} = Err ->
                    Err;
                {ok, V, _} ->
                    do_parse_attrs(Rest1, [V|Acc])
            end
    end;
do_parse_attrs(Rest, Acc) ->
    {ok, lists:reverse(Acc), Rest}.

-spec do_parse_attr(binary()) -> parse_result(attr()).
do_parse_attr(AttrLine) ->
    {AttrName, _} = Pair =
        case binary:split(AttrLine, <<":">>) of
            [N, V] -> {N, V};
            [N]    -> {N, novalue}
        end,

    case ersip_sdp_aux:check_token(AttrName) of
        false ->
            {error, {invalid_attr, AttrName}};
        true ->
            {ok, make_attr(Pair), <<>>}
    end.



-spec make_attr({attr_name(), attr_value() | novalue}) -> attr().
make_attr({Name, novalue}) ->
    Name;
make_attr({_, _} = Attr) ->
    Attr.

