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
         authenticate_result/2,
         authorize_result/2
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
                  authinfo :: undefined | authenticate_info(),
                  aoruri   :: undefined | ersip_uri:uri()
                 }).
-type request() :: #request{}.

-type request_phase() :: check_request
                       | authenticate
                       | authorize
                       | check_aor
                       | process_contacts
                       | find_bindings
                       | remove_bindings
                       | update_bindings
                       | prepare_answer
                       | terminated.

-type domain_set() :: any | gb_set:set(ersip_host:host()).
-type options()    :: #{check_aor_fun => check_aor_fun(),
                        authenticate  => boolean()
                       }.

-type registrar_se() :: {proxy,        ersip_uri:uri()}
                      | {reply,        ersip_sipmsg:sipmsg()}
                      | {authenticate, ersip_sipmsg:sipmsg()}
                      | {authorize,    authenticate_info(), AOR :: ersip_uri:uri()}
                      | clear_request.
-type request_result() :: {request(), registrar_se()}.
-type event() :: start.
-type authenticate_result() :: {ok, authenticate_info()}
                             | {error, term()}.
-type authenticate_info() :: term().

-type authorize_result() :: authorized | unauthorized.

-type check_aor_fun() :: fun((AOR :: ersip_uri:uri(), RURI :: ersip_uri:uri()) -> boolean()).

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
    continue(entry, Request).

%% Called when authentication result has got.
-spec authenticate_result(authenticate_result(), request()) -> request_result().
authenticate_result({authorized, _AuthInfo} = Event, #request{} = Request) ->
    continue(Event, Request);
authenticate_result({unauthorized, _ReplySipMsg} = Event, #request{} = Request) ->
    continue(Event, Request);
authenticate_result({error, _Reason} = Event, #request{} = Request) ->
    continue(Event, Request).

%% Called when authorization result has got.
-spec authorize_result(authorize_result(), request()) -> request_result().
authorize_result(authorized = Event, #request{} = Request) ->
    continue(Event, Request);
authorize_result(unauthorized = Event, #request{} = Request) ->
    continue(Event, Request).

%%%===================================================================
%%% Internal Implementation
%%%===================================================================

-define(default_options, #{authenticate => false,
                           check_aor_fun => fun(_AOR, _RURI) -> true end
                          }).

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
    authenticate(Event, Request);
continue(Event, #request{phase = authorize} = Request) ->
    authorize(Event, Request);
continue(Event, #request{phase = check_aor} = Request) ->
    check_aor(Event, Request);
continue(Event, #request{phase = process_contacts} = Request) ->
    process_contacts(Event, Request).

-spec set_phase(request_phase(), request()) -> request().
set_phase(Phase, #request{} = Request) ->
    Request#request{phase = Phase}.

%% 1. The registrar inspects the Request-URI to determine whether it
%%    has access to bindings for the domain identified in the
%%    Request-URI.  If not, and if the server also acts as a proxy
%%    server, the server SHOULD forward the request to the addressed
%%    domain, following the general behavior for proxying messages
%%    described in Section 16.
-spec check_request(event(), request()) -> request_result().
check_request(entry, #request{config = #config{domains = any}} = Request) ->
    continue(entry, set_phase(authenticate, Request));
check_request(entry, #request{config = #config{domains = Domains}, sipmsg = SipMsg} = Request) ->
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
            continue(entry, set_phase(authenticate, Request));
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

%% 3. A registrar SHOULD authenticate the UAC.  Mechanisms for the
%%    authentication of SIP user agents are described in Section 22.
%%    Registration behavior in no way overrides the generic
%%    authentication framework for SIP.  If no authentication
%%    mechanism is available, the registrar MAY take the From address
%%    as the asserted identity of the originator of the request.
-spec authenticate(event(), request()) -> request_result().
authenticate(entry, #request{config = #config{options = #{authenticate := false}}} = Request) ->
    continue(entry, set_phase(check_aor, Request));
authenticate(entry, #request{config = #config{options = #{authenticate := true}}, sipmsg = SipMsg} = Request) ->
    {Request, [{authenticate, SipMsg}]};
authenticate({authorized, AuthInfo}, #request{} = Request) ->
    Request1 = Request#request{authinfo = AuthInfo},
    continue(entry, set_phase(authorize, Request1));
authenticate({unauthorized, ReplySipMsg}, #request{} = Request) ->
    terminate_request({reply, ReplySipMsg}, Request);
authenticate({error, _Reason}, #request{sipmsg = SipMsg} = Request) ->
    ReplySipMsg = ersip_sipmsg:reply(500, SipMsg),
    terminate_request({reply, ReplySipMsg}, Request).


%% 4. The registrar SHOULD determine if the authenticated user is
%%    authorized to modify registrations for this address-of-record.
%%    For example, a registrar might consult an authorization database
%%    that maps user names to a list of addresses-of-record for which
%%    that user has authorization to modify bindings.  If the
%%    authenticated user is not authorized to modify bindings, the
%%    registrar MUST return a 403 (Forbidden) and skip the remaining
%%    steps.
-spec authorize(event(), request()) -> request_result().
authorize(entry, #request{authinfo = AuthInfo, sipmsg = SipMsg} = Request) ->
    AOR = ersip_sipmsg:get(to, SipMsg),
    AORURI = ersip_hdr_fromto:uri(AOR),
    {Request, [{authorize, AuthInfo, AORURI}]};
authorize(unauthorized, #request{sipmsg = SipMsg} = Request) ->
    ReplySipMsg = ersip_sipmsg:reply(403, SipMsg),
    terminate_request({reply, ReplySipMsg}, Request);
authorize(authrorized, #request{} = Request) ->
    continue(entry, set_phase(check_aor, Request)).


%% 5. The registrar extracts the address-of-record from the To header
%%    field of the request.  If the address-of-record is not valid
%%    for the domain in the Request-URI, the registrar MUST send a
%%    404 (Not Found) response and skip the remaining steps.  The URI
%%    MUST then be converted to a canonical form.  To do that, all
%%    URI parameters MUST be removed (including the user-param), and
%%    any escaped characters MUST be converted to their unescaped
%%    form.  The result serves as an index into the list of bindings.
-spec check_aor(event(), request()) ->request_result().
check_aor(entry, #request{} = Request0) ->
    #request{sipmsg = SipMsg,
             config = #config{options = #{check_aor_fun := CheckAORFun}}
            } = Request0,
    RURI = ersip_sipmsg:get(ruri, SipMsg),
    AOR = ersip_sipmsg:get(to, SipMsg),
    AORURI = ersip_hdr_fromto:uri(AOR),
    KeyAOR0 = ersip_uri:make_key(AORURI),
    KeyAOR1 = ersip_uri:clear_params(KeyAOR0),
    case CheckAORFun(KeyAOR1, RURI) of
        false ->
            ReplySipMsg = ersip_sipmsg:reply(404, SipMsg),
            terminate_request({reply, ReplySipMsg}, Request0);
        true ->
            Request1 = Request0#request{aoruri = KeyAOR1},
            continue(entry, set_phase(find_bindings, Request1))
    end.

%% 6. The registrar checks whether the request contains the Contact
%%    header field.  If not, it skips to the last step.  If the
%%    Contact header field is present, the registrar checks if there
%%    is one Contact field value that contains the special value "*"
%%    and an Expires field.  If the request has additional Contact
%%    fields or an expiration time other than zero, the request is
%%    invalid, and the server MUST return a 400 (Invalid Request) and
%%    skip the remaining steps.  If not, the registrar checks whether
%%    the Call-ID agrees with the value stored for each binding.  If
%%    not, it MUST remove the binding.  If it does agree, it MUST
%%    remove the binding only if the CSeq in the request is higher
%%    than the value stored for that binding.  Otherwise, the update
%%    MUST be aborted and the request fails.
-spec process_contacts(event(), request()) -> request_result().
process_contacts(entry, #request{sipmsg = SipMsg} = Request) ->
    case ersip_sipmsg:find(contact, SipMsg) of
        not_found ->
            continue(entry, set_phase(prepare_answer, Request));
        {ok, ContactHdrs} ->
            ok
    end.

-spec terminate_request(registrar_se(), request()) -> request_result().
terminate_request(Action, #request{} = Request) ->
    {set_phase(terminated, Request), [Action, clear_request]}.

