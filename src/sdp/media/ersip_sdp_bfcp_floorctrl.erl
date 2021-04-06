%%
%% Copyright (c) 2021 Dmitry Poroh
%% All rights reserved.
%% Distributed under the terms of the MIT License. See the LICENSE file.
%%
%% SDP media BFCP 'floorctrl' attribute
%%

-module(ersip_sdp_bfcp_floorctrl).

-export([new/2,
         client/1,
         server/1,
         set_client/2,
         set_server/2,
         parse/1,
         assemble/1,
         assemble_bin/1
        ]).

-export_type([bfcp_floorctrl/0]).

%%%===================================================================
%%% Types
%%%===================================================================
-record(bfcp_floorctrl, {client = false :: boolean(),
                         server = false :: boolean()}).

-type bfcp_floorctrl() :: #bfcp_floorctrl{}.

-type parse_result()  :: parse_result(bfcp_floorctrl()).
-type parse_result(T) :: ersip_parser_aux:parse_result(T).

%%%===================================================================
%%% API
%%%===================================================================
-spec new(boolean(), boolean()) -> bfcp_floorctrl().
new(C, S) ->
    #bfcp_floorctrl{client = C, server = S}.

-spec client(bfcp_floorctrl()) -> boolean().
client(#bfcp_floorctrl{client = C}) ->
    C.

-spec server(bfcp_floorctrl()) -> boolean().
server(#bfcp_floorctrl{server = S}) ->
    S.

-spec set_client(boolean(), bfcp_floorctrl()) -> bfcp_floorctrl().
set_client(C, #bfcp_floorctrl{} = BF) ->
    BF#bfcp_floorctrl{client = C}.

-spec set_server(boolean(), bfcp_floorctrl()) -> bfcp_floorctrl().
set_server(S, #bfcp_floorctrl{} = BF) ->
    BF#bfcp_floorctrl{server = S}.

-spec parse(binary()) -> parse_result().
parse(Bin) ->
    do_parse_bfcp_floorctrl(Bin).

-spec assemble(bfcp_floorctrl()) -> iolist().
assemble(BF) ->
    assemble_bfcp_floorctrl(BF).

-spec assemble_bin(bfcp_floorctrl()) -> binary().
assemble_bin(BF) ->
    iolist_to_binary(assemble(BF)).

%%%===================================================================
%%% Internal implementation
%%%===================================================================
-define(sp, " ").

%% https://tools.ietf.org/rfc/rfc8856.html#name-sdp-floorctrl-attribute
%%
%% floor-control = role *(SP role)
%% role = "c-only" / "s-only" / "c-s"
%%
-spec do_parse_bfcp_floorctrl(binary()) -> parse_result().
do_parse_bfcp_floorctrl(Bin) ->
    Tokens = binary:split(Bin, <<?sp>>, [global]),
    case parse_roles(Tokens) of
        {ok, _BF, _Rest} = OK ->
            OK;
        {error, Reason} ->
            {error, {invalid_bfcp_floorctrl, Reason}}
    end.

-spec assemble_bfcp_floorctrl(bfcp_floorctrl()) -> iolist().
assemble_bfcp_floorctrl(#bfcp_floorctrl{
                          server = S,
                          client = C}) ->
    lists:join(?sp, assemble_server(S) ++ assemble_client(C)).

-spec parse_roles([binary()]) -> parse_result().
parse_roles(Tokens) ->
    parse_roles(Tokens, #bfcp_floorctrl{}).

-spec parse_roles([binary()], bfcp_floorctrl()) -> parse_result().
parse_roles([<<"c-s">>|Tokens], BF) ->
    BF1 = BF#bfcp_floorctrl{
            client = true,
            server = true},
    parse_roles(Tokens, BF1);
parse_roles([<<"s-only">>|Tokens], BF) ->
    BF1 = BF#bfcp_floorctrl{server = true},
    parse_roles(Tokens, BF1);
parse_roles([<<"c-only">>|Tokens], BF) ->
    BF1 = BF#bfcp_floorctrl{client = true},
    parse_roles(Tokens, BF1);
parse_roles([Junk|_], _) ->
    {error, {invalid_role, Junk}};
parse_roles([], BF) ->
    {ok, BF, <<>>}.

assemble_server(false) ->
    [];
assemble_server(true) ->
    [<<"s-only">>].

assemble_client(false) ->
    [];
assemble_client(true) ->
    [<<"c-only">>].
