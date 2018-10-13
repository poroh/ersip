%%
%% Copyright (c) 2018 Dmitry Poroh
%% All rights reserved.
%% Distributed under the terms of the MIT License. See the LICENSE file.
%%
%% SIP Registrar Side effects
%%

-module(ersip_registrar_se).

-export([proxy/1,
         reply/1,
         authenticate/1,
         authorize/2,
         find_bindings/1,
         update_bindings/4
        ]).
-export_type([side_effect/0]).

%%%===================================================================
%%% Types
%%%===================================================================

-type side_effect() :: proxy()
                     | reply()
                     | authenticate()
                     | authorize()
                     | find_bindings()
                     | update_bindings().

%% Proxy request to the destination defined by URI.
-type proxy()         :: {proxy, ersip_uri:uri()}.
%% Reply to the request and terminate request.
-type reply()         :: {reply, ersip_sipmsg:sipmsg()}.

%% Authenticate user sent request and provide authentication info.
-type authenticate()  :: {authenticate, ersip_sipmsg:sipmsg()}.

%% Authorize request of AOR modification by user authentication info.
-type authorize()     :: {authorize, authenticate_info(), AOR :: ersip_uri:uri()}.

%% Find all binding related to AOR
-type find_bindings() :: {find_bindings, AOR :: ersip_uri:uri()}.

%% Update bindings related to AOR:
-type update_bindings() :: {update_bindings, AOR :: ersip_uri:uri(), update_descr()}.

-type update_descr() :: {Added   :: binding_list(),
                         Updated :: binding_list(),
                         Removed :: binding_list()}.

-type binding_list() :: [ersip_registrar_binding:binding()].
-type authenticate_info() :: term().

%%%===================================================================
%%% API
%%%===================================================================

-spec proxy(ersip_uri:uri()) -> proxy().
proxy(RURI) ->
    {proxy, RURI}.

-spec reply(ersip_sipmsg:sipmsg()) -> reply().
reply(SipMsg) ->
    {reply, SipMsg}.

-spec authenticate(ersip_sipmsg:sipmsg()) -> authenticate().
authenticate(SipMsg) ->
    {authenticate, SipMsg}.

-spec authorize(authenticate_info(), AOR :: ersip_uri:uri()) -> authorize().
authorize(AuthInfo, AOR) ->
    {authorize, AuthInfo, AOR}.

-spec find_bindings(AOR :: ersip_uri:uri()) -> find_bindings().
find_bindings(AOR) ->
    {find_bindings, AOR}.

-spec update_bindings(AOR, Added, Updated, Removed) -> update_bindings() when
      AOR :: ersip_uri:uri(),
      Added   :: {added,   binding_list()},
      Updated :: {updated, binding_list()},
      Removed :: {removed, binding_list()}.
update_bindings(AOR, {added, Added}, {updated, Updated}, {removed, Removed}) ->
    {update_bindings, AOR, {Added, Updated, Removed}}.
