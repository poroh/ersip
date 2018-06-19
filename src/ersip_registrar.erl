%%
%% Copyright (c) 2018 Dmitry Poroh
%% All rights reserved.
%% Distributed under the terms of the MIT License. See the LICENSE file.
%%
%% SIP Registrar
%%

-module(ersip_registrar).

-export([new_config/2,
         new_request/2,
         authenticate_result/2
        ]).

%%%===================================================================
%%% Types
%%%===================================================================

-record(config, {domains        :: domain_set(),
                 options        :: options()
                }).
-type config() :: #config{}.

-record(request, {config   :: config(),
                  phase    :: request_phase(),
                  sipmsg   :: ersip_sipmsg:sipmsg(),
                  authinfo :: authenticate_info()
                 }).
-type request() :: #request{}.

-type request_phase() :: check_request
                       | authenticate
                       | authorize
                       | check_aor
                       | find_bindings
                       | remove_bindings
                       | update_bindings
                       | prepare_answer
                       | terminated.

-type domain_set() :: any | gb_set:set(ersip_host:host()).
-type options()    :: #{}.

-type registrar_se() :: {proxy,        ersip_uri:uri()}
                      | {reply,        ersip_sipmsg:sipmsg()}
                      | {authenticate, ersip_sipmsg:sipmsg()}
                      | clear_request.
-type request_result() :: {request(), registrar_se()}.
-type event() :: start.
-type authenticate_result() :: {ok, authenticate_info()}
                             | {error, term()}.
-type authenticate_info() :: term().

%%%===================================================================
%%% API
%%%===================================================================

-spec new_config([ersip_host:host()] | any, options()) -> config().
new_config(DomainList, Options) ->
    new_config_impl(DomainList, Options).

%% Called when registrar receives new request.
-spec new_request(ersip_sipmsg:sipmsg(), config()) -> request_result().
new_request(SipMsg, #config{} = Config) ->
    Request =
        #request{sipmsg = SipMsg,
                 phase  = check_request,
                 config = Config},
    continue(start, Request).

%% Called when request is authentincated
-spec authenticate_result(authenticate_result(), request()) -> request_result().
authenticate_result({ok, AuthInfo}, #request{} = Request) ->
    continue({auth_info, AuthInfo}, Request).

%%%===================================================================
%%% Internal Implementation
%%%===================================================================

-define(default_options, #{authenticate => false}).

-spec new_config_impl([ersip_host:host()] | any, options()) -> config().
new_config_impl(DomainList, Options) ->
    Domains =
        case DomainList of
            any ->
                any;
            List when is_list(List) ->
                Translated = [ersip_host:make_key(H) || H <- List],
                gb_sets:from_list(Translated)
        end,
    #config{domains      = Domains,
            options      = maps:merge(?default_options, Options)
           }.
-spec continue(event(), request()) -> request_result().
continue(Event, #request{phase = check_request} = Request) ->
    check_request(Event, Request);
continue(Event, #request{phase = authenticate} = Request) ->
    authenticate(Event, Request).

-spec set_phase(request_phase(), request()) -> request().
set_phase(Phase, #request{} = Request) ->
    Request#request{phase = Phase}.

-spec check_request(event(), request()) -> request_result().
check_request(start, #request{config = #config{domains = any}} = Request) ->
    continue(start, set_phase(authorize, Request));
check_request(start, #request{config = #config{domains = Domains}, sipmsg = SipMsg} = Request) ->
    %% The registrar inspects the Request-URI to determine whether it
    %% has access to bindings for the domain identified in the
    %% Request-URI.  If not, and if the server also acts as a proxy
    %% server, the server SHOULD forward the request to the addressed
    %% domain, following the general behavior for proxying messages
    %% described in Section 16.
    RURI = ersip_sipmsg:ruri(SipMsg),
    CheckResult =
        case ersip_uri:scheme(RURI) of
            {scheme, sip} ->
                check_domain(ersip_uri:get(host, RURI), Domains);
            {scheme, sips} ->
                check_domain(ersip_uri:get(host, RURI), Domains);
            {scheme, _} ->
                proxy
        end,
    case CheckResult of
        continue ->
            continue(start, set_phase(authenticate, Request));
        proxy ->
            terminate_request({proxy, RURI}, Request)
    end.

-spec check_domain(ersip_host:host(), domain_set()) -> continue | proxy.
check_domain(Host, Domains) ->
    case gb_sets:is_element(ersip_host:make_key(Host), Domains) of
        true ->
            continue;
        false ->
            proxy
    end.

-spec authenticate(event(), request()) -> request_result().
authenticate(start, #request{config = #config{options = #{authenticate := false}}} = Request) ->
    continue(start, set_phase(check_aor, Request));
authenticate(start, #request{config = #config{options = #{authenticate := true}}, sipmsg = SipMsg} = Request) ->
    {Request, [{authenticate, SipMsg}]};
authenticate({ok, AuthInfo}, #request{sipmsg = SipMsg} = Request) ->
    ok;
authenticate({error, Reason}, #request{sipmsg = SipMsg} = Request) ->
    ok.

-spec terminate_request(registrar_se(), request()) -> request_result().
terminate_request(Action, #request{} = Request) ->
    {set_phase(terminated, Request), [Action, clear_request]}.

