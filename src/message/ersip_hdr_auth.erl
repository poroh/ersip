%%%
%%% Copyright (c) 2018, 2020 Dmitry Poroh
%%% All rights reserved.
%%% Distributed under the terms of the MIT License. See the LICENSE file.
%%%
%%% SIP Headers:
%%% - WWW-Authenticate
%%% - Authorization
%%% - Proxy-Authenticate
%%% - Proxy-Authorization
%%%

-module(ersip_hdr_auth).

-export([make/1,
         parse/1,
         build/2,
         raw/1
        ]).

-export_type([headers/0, raw/0]).

%%===================================================================
%% Types
%%===================================================================

-type headers() :: [ersip_authinfo:authinfo()].
-type parse_result() :: {ok, headers()}
                      | {error, term()}.
-type raw() :: [ersip_authinfo:raw()].

%%===================================================================
%% API
%%===================================================================

-spec make(raw()) -> headers().
make(AuthInfoRawList) ->
    [ersip_authinfo:make(AIRaw) || AIRaw <- AuthInfoRawList].

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

-spec raw(headers()) -> raw().
raw(Headers) ->
    [ersip_authinfo:raw(H) || H <- Headers].

%%===================================================================
%% Internal implementation
%%===================================================================

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
