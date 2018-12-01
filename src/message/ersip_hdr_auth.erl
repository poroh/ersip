%%
%% Copyright (c) 2018 Dmitry Poroh
%% All rights reserved.
%% Distributed under the terms of the MIT License. See the LICENSE file.
%%
%% SIP Headers:
%% - WWW-Authenticate
%% - Authorization
%% - Proxy-Authenticate
%% - Proxy-Authorization
%%

-module(ersip_hdr_auth).

-export([parse/1,
         build/2
        ]).

%%%===================================================================
%%% Types
%%%===================================================================

-type headers() :: [ersip_authinfo:authinfo()].
-type parse_result() :: {ok, headers()}
                      | {error, term()}.

%%%===================================================================
%%% API
%%%===================================================================

-spec parse(ersip_hdr:header()) -> parse_result().
parse(Header) ->
    HdrValues = [iolist_to_binary(V) || V <- ersip_hdr:raw_values(Header)],
    parse_impl(HdrValues, []).

-spec build(HdrName :: binary(), headers()) -> ersip_hdr:header().
build(HdrName, AuthInfos) when is_list(AuthInfos) ->
    Hdr = ersip_hdr:new(HdrName),
    lists:foldl(fun(AI, H) ->
                        Val = ersip_authinfo:assemble(AI),
                        ersip_hdr:add_value(Val, H)
                end,
                Hdr,
                AuthInfos).

%%%===================================================================
%%% Internal implementation
%%%===================================================================

-spec parse_impl([binary()], headers()) -> parse_result().
parse_impl([], Acc) ->
    {ok, lists:reverse(Acc)};
parse_impl([V | Rest], Acc) ->
    case ersip_authinfo:parse(V) of
        {ok, AI} ->
            parse_impl(Rest, [AI | Acc]);
        {error, _} = Error ->
            Error
    end.
