%%
%% Copyright (c) 2020 Dmitry Poroh
%% All rights reserved.
%% Distributed under the terms of the MIT License. See the LICENSE file.
%%
%% SDP media RTCP Feedback Capability Attribute
%%

-module(ersip_sdp_attr_rtcp_fb).

-export([type/1,
         set_type/2,
         val/1,
         set_val/2,
         param/1,
         set_param/2,
         bytestring/1,
         set_bytestring/2,
         parse/1,
         assemble/1,
         assemble_bin/1
        ]).

-export_type([rtcp_fb/0]).

%%%===================================================================
%%% Types
%%%===================================================================

-record(rtcp_fb, {
    type                   :: rtcp_fb_tp(),
    val                    :: rtcp_fb_val(),
    param                  :: undefined | rtcp_fb_params(),
    bytestring             :: undefined | binary()
}).

-type rtcp_fb()            :: #rtcp_fb{}.
-type rtcp_fb_tp()         :: non_neg_integer() | '*'.
-type rtcp_fb_val()        :: ack | nack | 'trr-int' | rtcp_fb_id().
-type rtcp_fb_id()         :: binary().
-type rtcp_fb_params()     :: non_neg_integer() | rtcp_fb_param() | rtcp_fb_ack_param() | rtcp_fb_nack_param().
-type rtcp_fb_param()      :: app | binary().
-type rtcp_fb_ack_param()  :: rpsi | rtcp_fb_param().
-type rtcp_fb_nack_param() :: pli | sli | rtcp_fb_nack_param().

-type parse_result()       :: ersip_parser_aux:parse_result(rtcp_fb()).
-type parse_result(X)      :: ersip_parser_aux:parse_result(X).

%%%===================================================================
%%% API
%%%===================================================================

-spec type(rtcp_fb()) -> rtcp_fb_tp().
type(#rtcp_fb{type = T}) ->
    T.

-spec val(rtcp_fb()) -> rtcp_fb_val().
val(#rtcp_fb{val = V}) ->
    V.

-spec param(rtcp_fb()) -> rtcp_fb_params().
param(#rtcp_fb{param = P}) ->
    P.

-spec bytestring(rtcp_fb()) -> binary().
bytestring(#rtcp_fb{bytestring = B}) ->
    B.

-spec set_type(rtcp_fb_tp(), rtcp_fb()) -> rtcp_fb().
set_type(T, Rtcp) ->
    Rtcp#rtcp_fb{type = T}.

-spec set_val(rtcp_fb_val(), rtcp_fb()) -> rtcp_fb().
set_val(V, Rtcp) ->
    Rtcp#rtcp_fb{val = V}.

-spec set_param(rtcp_fb_params(), rtcp_fb()) -> rtcp_fb().
set_param(P, Rtcp) ->
    Rtcp#rtcp_fb{param = P}.

-spec set_bytestring(binary(), rtcp_fb()) -> rtcp_fb().
set_bytestring(B, Rtcp) ->
    Rtcp#rtcp_fb{bytestring = B}.



-spec parse(binary()) -> parse_result().
parse(Bin) ->
    do_parse_rtcp_fb(Bin).

-spec assemble(rtcp_fb()) -> iolist().
assemble(RtcpFb) ->
    assemble_rtcp_fb(RtcpFb).

-spec assemble_bin(rtcp_fb()) -> binary().
assemble_bin(RtcpFb) ->
    iolist_to_binary(assemble(RtcpFb)).

%%%===================================================================
%%% Internal implementation
%%%===================================================================

-define(sp, " ").




%% https://datatracker.ietf.org/doc/html/rfc4585#page-23
%%
%%      rtcp-fb-syntax = "a=rtcp-fb:" rtcp-fb-pt SP rtcp-fb-val CRLF
%%
%%      rtcp-fb-pt         = "*"   ; wildcard: applies to all formats
%%                         / fmt   ; as defined in SDP spec
%%
%%      rtcp-fb-val        = "ack" rtcp-fb-ack-param
%%                         / "nack" rtcp-fb-nack-param
%%                         / "trr-int" SP 1*DIGIT
%%                         / rtcp-fb-id rtcp-fb-param
%%
%%      rtcp-fb-id         = 1*(alpha-numeric / "-" / "_")
%%
%%      rtcp-fb-param      = SP "app" [SP byte-string]
%%                         / SP token [SP byte-string]
%%                         / ; empty
%%
%%      rtcp-fb-ack-param  = SP "rpsi"
%%                         / SP "app" [SP byte-string]
%%                         / SP token [SP byte-string]
%%                         / ; empty
%%
%%      rtcp-fb-nack-param = SP "pli"
%%                         / SP "sli"
%%                         / SP "rpsi"
%%                         / SP "app" [SP byte-string]
%%                         / SP token [SP byte-string]
%%                         / ; empty
%%
-spec do_parse_rtcp_fb(binary()) -> parse_result().
do_parse_rtcp_fb(<<Rest/binary>>) ->
    Parsers = [fun parse_ptype/1,                           %% rtcp-fb-pt
               fun ersip_parser_aux:parse_lws/1,
               fun parse_val/1,                             %% rtcp-fb-val
               fun ersip_parser_aux:trim_lws/1
              ],
    case ersip_parser_aux:parse_all(Rest, Parsers) of
        {ok, Result, Rest1} ->
            [Type, _, Val, _] = Result,

            R1 = #rtcp_fb{
                type = Type,
                val = Val
            },

            case parse_param(Rest1, R1) of
                {error, Reason} ->
                    {error, {invalid_rtcp_fb, Reason}};
                {<<>>, R2} ->
                    {ok, R2, <<>>};
                {Rest2, _} ->
                    {error, {invalid_rtcp_fb, {unexpected_input, Rest2}}}
            end;

        {error, Reason} ->
            {error, {invalid_rtcp_fb, Reason}}
    end.

-spec parse_ptype(binary()) -> parse_result(rtcp_fb_tp()).
parse_ptype(<<"*", ?sp, Rest/binary>>) ->
    {ok, '*', <<?sp, Rest/binary>>};
parse_ptype(<<Rest/binary>>) ->
    ersip_parser_aux:parse_pos_int(Rest).


-spec parse_val(binary()) -> parse_result(rtcp_fb_tp()).
parse_val(<<"ack">>) ->
    {ok, ack, <<>>};
parse_val(<<"ack", ?sp, Rest/binary>>) ->
    {ok, ack, Rest};
parse_val(<<"nack">>) ->
    {ok, nack, <<>>};
parse_val(<<"nack", ?sp, Rest/binary>>) ->
    {ok, nack, Rest};
parse_val(<<"trr-int", ?sp, Rest/binary>>) ->
    {ok, 'trr-int', Rest};
parse_val(<<Rest/binary>>) ->
    parse_id(Rest).

-spec parse_param(binary(), rtcp_fb()) -> {error, term()} | {binary(), rtcp_fb()}.
parse_param(<<>>, R1) ->
    {<<>>, R1};

parse_param(Bin, R1 = #rtcp_fb{val = 'trr-int'}) ->
    case ersip_parser_aux:parse_pos_int(Bin) of
        {ok, I, Rest} ->
            {Rest, R1#rtcp_fb{param = I}};
        Err -> Err
    end;

parse_param(<<"pli">>, #rtcp_fb{val = nack} = R1) ->
    {<<>>, R1#rtcp_fb{param = pli}};
parse_param(<<"sli">>, #rtcp_fb{val = nack} = R1) ->
    {<<>>, R1#rtcp_fb{param = sli}};
parse_param(<<"rpsi">>, #rtcp_fb{val = V} = R1) when V == nack; V == ack ->
    {<<>>, R1#rtcp_fb{param = rpsi}};
parse_param(<<"pli", ?sp, Rest/binary>>, #rtcp_fb{val = nack} = R1) ->
    {Rest, R1#rtcp_fb{param = pli}};
parse_param(<<"sli", ?sp, Rest/binary>>, #rtcp_fb{val = nack} = R1) ->
    {Rest, R1#rtcp_fb{param = sli}};
parse_param(<<"rpsi", ?sp, Rest/binary>>, #rtcp_fb{val = V} = R1) when V == nack; V == ack ->
    {Rest, R1#rtcp_fb{param = rpsi}};

parse_param(<<"pli">>, _) ->
    {error, {unexpected_input, <<"pli">>}};
parse_param(<<"pli", ?sp,  _/binary>>, _) ->
    {error, {unexpected_input, <<"pli">>}};
parse_param(<<"sli">>, _) ->
    {error, {unexpected_input, <<"sli">>}};
parse_param(<<"sli", ?sp,  _/binary>>, _) ->
    {error, {unexpected_input, <<"sli">>}};
parse_param(<<"rpsi">>, _) ->
    {error, {unexpected_input, <<"rpsi">>}};
parse_param(<<"rpsi", ?sp,  _/binary>>, _) ->
    {error, {unexpected_input, <<"rpsi">>}};

parse_param(<<"app">>, R1) ->
    {<<>>, R1#rtcp_fb{param = app}};
parse_param(<<"app", ?sp, Rest/binary>>, R1) ->
    {<<>>, R1#rtcp_fb{param = app, bytestring = Rest}};

parse_param(<<Rest/binary>>, R1) ->
    case ersip_sdp_aux:parse_token(Rest) of
        {ok, T, <<>>} ->
            {<<>>, R1#rtcp_fb{param = T}};
        {ok, T, <<?sp, Rest1/binary>>} ->
            {<<>>, R1#rtcp_fb{param = T, bytestring = Rest1}};
        {ok, T, Rest1} ->
            {error, {unexpected_input, Rest1}};
        Err ->
            Err
    end.




-spec assemble_rtcp_fb(rtcp_fb()) -> iolist().
assemble_rtcp_fb(#rtcp_fb{} = RtcpFb) ->
    #rtcp_fb{
        type = Type,
        val = Val
    } = RtcpFb,

    [to_iolist_item(Type),?sp,
     to_iolist_item(Val),
     param_to_iolist(RtcpFb)].

-spec param_to_iolist(rtcp_fb()) -> iolist().
param_to_iolist(#rtcp_fb{param = undefined}) ->
    [];
param_to_iolist(#rtcp_fb{param = Param, bytestring = undefined}) ->
    [?sp, to_iolist_item(Param)];
param_to_iolist(#rtcp_fb{param = Param, bytestring = ByteString}) ->
    [?sp, to_iolist_item(Param), ?sp, to_iolist_item(ByteString)].


%%%===================================================================
%%% Internal implementation
%%%===================================================================

to_iolist_item(N) when is_number(N) -> integer_to_binary(N);
to_iolist_item(A) when is_atom(A) -> atom_to_binary(A);
to_iolist_item(O) -> O.

-spec parse_id(binary()) -> ersip_parser_aux:parse_result(binary()).
parse_id(<<>>) ->
    {error, id_expected};
parse_id(<<C, _/binary>> = Bin) ->
    case is_id_char(C) of
        true ->
            case find_id_end(Bin, 0) of
                {error, _}  = Err ->
                    Err;
                TokenEnd ->
                    <<Token:TokenEnd/binary, Rest/binary>> = Bin,
                    {ok, Token, Rest}
            end;
        false ->
            {error, {invalid_id_char, <<C:8>>}}
    end.

-spec is_id_char(char()) -> boolean().
is_id_char(C) when C >= $0, C =< $9 ->
    true;
is_id_char(C) when C >= $a, C =< $z ->
    true;
is_id_char(C) when C >= $A, C =< $Z ->
    true;
is_id_char($-) ->
    true;
is_id_char($_) ->
    true;
is_id_char(_) ->
    false.

-spec find_id_end(binary(), non_neg_integer()) -> non_neg_integer().
find_id_end(<<>>, Acc) -> Acc;
find_id_end(<<?sp, _/binary>>, Acc) -> Acc;
find_id_end(<<C:8, Rest/binary>>, Acc) ->
    case is_id_char(C) of
        false ->
            {error, {invalid_id_char, <<C:8>>}};
        true ->
            find_id_end(Rest, Acc+1)
    end.
