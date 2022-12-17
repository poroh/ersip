%%%
%%% Copyright (c) 2020 Dmitry Poroh
%%% All rights reserved.
%%% Distributed under the terms of the MIT License. See the LICENSE file.
%%%
%%% Display name routines
%%%

-module(ersip_display_name).

-export([empty/0,
         make/1,
         parse_dn/1,
         assemble/1,
         assemble_bin/1,
         raw/1
        ]).

-export_type([display_name/0, raw/0, make/0]).

-include("ersip_sip_abnf.hrl").

%%===================================================================
%% Types
%%===================================================================

-type display_name() :: {display_name, binary() | [binary()]}.
-type raw() :: binary().
-type make() :: binary().

%%===================================================================
%% API
%%===================================================================

%% @doc Create empty display name.
-spec empty() -> display_name().
empty() ->
    {display_name, []}.

%% @doc Create display name from binary.
-spec make(make()) -> display_name().
make(Binary) ->
    case parse_dn(Binary) of
        {ok, DN, <<>>} -> DN;
        {ok, _, _} ->
            {display_name, ersip_quoted_string:quote(Binary)};
        {error, _} ->
            {display_name, ersip_quoted_string:quote(Binary)}
    end.

%% @doc Parse dispalay name and return rest of the string.
%%
%% ABNF grammar part:
%% ```
%% display-name   =  *(token LWS)/ quoted-string
%% '''
-spec parse_dn(binary()) -> ersip_parser_aux:parse_result(display_name()).
parse_dn(<<>>) ->
    {ok, empty(), <<>>};
parse_dn(<<$", _/binary>> = Quoted) ->
    case ersip_parser_aux:quoted_string(Quoted) of
        {ok, Q, Rest} -> {ok, {display_name, Q}, Rest};
        error -> {error, {invalid_display_name, Quoted}}
    end;
parse_dn(Other) ->
    case ersip_parser_aux:token_list(Other, lws) of
        {ok,  Tokens, Rest} -> {ok, {display_name,  Tokens}, Rest};
        error -> {error, {invalid_display_name, Other}}
    end.


%% @doc Assemble display name to iolist.
-spec assemble(display_name()) -> iolist().
assemble({display_name, L}) when is_list(L) ->
    ersip_iolist:join(<<" ">>, L);
assemble({display_name, V}) when is_binary(V) ->
    V.

%% @doc Assemble display name to binary.
-spec assemble_bin(display_name()) -> binary().
assemble_bin({display_name, _} = DN) ->
    iolist_to_binary(assemble(DN)).

%% @doc Raw representation of display name. Note that it is unquoted.
%%
%% Examples:
%% ```
%%   <<"Alice">> = ersip_display_name:raw(ersip_display_name:make(<<"\"Alice\"">>)).
%%   <<"Lewis Carroll">> = ersip_display_name:raw(ersip_display_name:make(<<"Lewis Carroll">>)).
%%   <<"Lewis Carroll">> = ersip_display_name:raw(ersip_display_name:make(<<"\"Lewis Carroll\"">>)).
%%   <<"Theodore \"Teddy\" Roosevelt">> = ersip_display_name:raw(ersip_display_name:make(<<"Theodore \"Teddy\" Roosevelt">>)).
%% '''
-spec raw(display_name()) -> raw().
raw({display_name, L}) when is_list(L) ->
    iolist_to_binary(ersip_iolist:join(<<" ">>, L));
raw({display_name, DN}) when is_binary(DN) ->
    {ok, Unquoted, <<>>} = ersip_parser_aux:unquoted_string(DN),
    Unquoted.
