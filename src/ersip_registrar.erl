%%
%% Copyright (c) 2018 Dmitry Poroh
%% All rights reserved.
%% Distributed under the terms of the MIT License. See the LICENSE file.
%%
%% SIP Registrar function
%%
%% This file mostly implements functions decribed in p.p. 10.3 of
%% RFC3261.
%%

-module(ersip_registrar).

-export([new_config/2,
         new_request/2,
         authenticate_result/2,
         authorize_result/2,
         lookup_result/2,
         update_result/2,
         is_terminated/1
        ]).

%%%===================================================================
%%% Types
%%%===================================================================

%% --------------------
%% Registrar configuration
-record(config, {domains        :: domain_set(),
                 options        :: options()
                }).
-type config() :: #config{}.

-type domain_set()    :: any | gb_sets:set(ersip_host:host()).
-type options()       :: #{check_aor_fun   => check_aor_fun(),
                           authenticate    => boolean(),
                           to_tag          => auto | ersip_hdr_fromto:tag(),
                           default_expires => pos_integer(),
                           min_expires     => non_neg_integer()
                          }.
-type check_aor_fun() :: fun((AOR :: ersip_uri:uri(), RURI :: ersip_uri:uri()) -> boolean()).

%% --------------------
%% Request-related types
-record(request, {config   :: config(),
                  phase    :: request_phase(),
                  sipmsg   :: ersip_sipmsg:sipmsg(),
                  authinfo :: undefined | authenticate_info(),
                  aoruri   :: undefined | ersip_uri:uri(),
                  action   :: undefined | request_action(),
                  result_bindings :: undefined | [ersip_registrar_binding:binding()]
                 }).
-type request() :: #request{}.

-type request_phase() :: check_request
                       | authenticate
                       | authorize
                       | check_aor
                       | process_contacts
                       | process_bindings
                       | update_bindings
                       | prepare_answer
                       | terminated.

%% Return value of all request-related functions.
-type request_result() :: {request(), ersip_registrar_se:side_effect()}.

-type request_action()    :: remove_all_bindings
                           | {update_bindings, [exp_binding()]}
                           | request_all_bindings.
-type authenticate_info() :: term().
-type exp_binding()       :: {Expires :: non_neg_integer(), ersip_hdr_contact:contact()}.

%% --------------------
%% Internal events:
-type event() :: entry
               | authenticate_result_event()
               | authorize_result_event()
               | lookup_result_event()
               | update_result_event()
               | update_event().

-type authenticate_result_event() :: {authenticate_result, authenticate_result()}.
-type authorize_result_event()    :: {authorize_result, authorize_result()}.
-type lookup_result_event()       :: {lookup_result, lookup_result()}.
-type update_result_event()       :: {update_result, update_result()}.
-type update_event()              :: {update, update_map()}.
-type update_map()                :: #{added   := [ersip_registrar_binding:binding()],
                                       removed := [ersip_registrar_binding:binding()],
                                       updated := [ersip_registrar_binding:binding()]
                                      }.
%% --------------------
%% Side effects results:
-type authenticate_result() :: {ok, {authorized, authenticate_info()}}
                             | {ok, {unauthorized, Reply :: ersip_sipmsg:sipmsg()}}
                             | {error, term()}.
-type authorize_result()    :: {ok, authorized}
                             | {ok, unauthorized}
                             | {error, term()}.
-type lookup_result()       :: {ok, [ersip_registrar_binding:binding()]}
                             | {error, term()}.
-type update_result()       :: ok
                             | {error, term()}.

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
authenticate_result({ok, {authorized, _AuthInfo}} = Event, #request{} = Request) ->
    continue({authenticate_result, Event}, Request);
authenticate_result({ok, {unauthorized, _ReplySipMsg}} = Event, #request{} = Request) ->
    continue({authenticate_result, Event}, Request);
authenticate_result({error, _Reason} = Event, #request{} = Request) ->
    continue({authenticate_result, Event}, Request).

%% Called when authorization result has got.
-spec authorize_result(authorize_result(), request()) -> request_result().
authorize_result({ok, authorized} = Event, #request{} = Request) ->
    continue({authorize_result, Event}, Request);
authorize_result({ok, unauthorized} = Event, #request{} = Request) ->
    continue({authorize_result, Event}, Request);
authorize_result({error, _Reason} = Event, #request{} = Request) ->
    continue({authorize_result, Event}, Request).

-spec lookup_result(lookup_result(), request()) -> request_result().
lookup_result({ok, _} = OkResult, #request{} = Request) ->
    continue({lookup_result, OkResult}, Request);
lookup_result({error, _} = ErrResult, #request{} = Request) ->
    continue({lookup_result, ErrResult}, Request).

-spec update_result(update_result(), request()) -> request_result().
update_result(ok, #request{} = Request) ->
    continue({update_result, ok}, Request);
update_result({error, _Reason} = Error, #request{} = Request) ->
    continue({update_result, Error}, Request).

-spec is_terminated(request()) -> boolean().
is_terminated(#request{phase = terminated}) ->
    true;
is_terminated(#request{}) ->
    false.

%%%===================================================================
%%% Internal Implementation
%%%===================================================================

-define(default_options, #{authenticate    => false,
                           check_aor_fun   => fun(_AOR, _RURI) -> true end,
                           to_tag          => auto,
                           default_expires => 3600,
                           min_expires     => 10
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
    process_contacts(Event, Request);
continue(Event, #request{phase = process_bindings} = Request) ->
    process_bindings(Event, Request);
continue(Event, #request{phase = update_bindings} = Request) ->
    update_bindings(Event, Request);
continue(Event, #request{phase = prepare_answer} = Request) ->
    prepare_answer(Event, Request);
continue(Event, #request{phase = terminated}) ->
    error({unexpected_event, Event}).

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
            next_phase(authenticate, Request);
        proxy ->
            terminate_request({proxy, RURI}, Request)
    end.

-spec check_domain({host, ersip_host:host()}, domain_set()) -> continue | proxy.
check_domain({host, Host}, Domains) ->
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
    {Request, ersip_registrar_se:authenticate(SipMsg)};
authenticate({authenticate_result, {ok, {authorized, AuthInfo}}}, #request{} = Request) ->
    Request1 = Request#request{authinfo = AuthInfo},
    next_phase(authorize, Request1);
authenticate({authenticate_result, {ok, {unauthorized, ReplySipMsg}}}, #request{} = Request) ->
    terminate_request({reply, ReplySipMsg}, Request);
authenticate({authenticate_result, {error, _Reason}}, #request{sipmsg = SipMsg} = Request) ->
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
authorize({authorize_result, {ok, unauthorized}}, #request{sipmsg = SipMsg} = Request) ->
    ReplySipMsg = ersip_sipmsg:reply(403, SipMsg),
    terminate_request({reply, ReplySipMsg}, Request);
authorize({authorize_result, {ok, authorized}}, #request{} = Request) ->
    next_phase(check_aor, Request);
authorize({authorize_result, {error, _}}, #request{sipmsg = SipMsg} = Request) ->
    ReplySipMsg = ersip_sipmsg:reply(500, SipMsg),
    terminate_request({reply, ReplySipMsg}, Request).

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
    RURI = ersip_sipmsg:ruri(SipMsg),
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
            next_phase(process_contacts, Request1)
    end.

-spec process_contacts(event(), request()) -> request_result().
process_contacts(entry, #request{sipmsg = SipMsg} = Request) ->
    case ersip_sipmsg:find(contact, SipMsg) of
        not_found ->
            Request1 = Request#request{action = request_all_bindings},
            next_phase(process_bindings, Request1);
        {ok, star} ->
            %% 6. The registrar checks whether the request contains
            %%    the Contact header field.  If not, it skips to the
            %%    last step.  If the Contact header field is present,
            %%    the registrar checks if there is one Contact field
            %%    value that contains the special value "*" and an
            %%    Expires field.  If the request has additional
            %%    Contact fields or an expiration time other than
            %%    zero, the request is invalid, and the server MUST
            %%    return a 400 (Invalid Request) and skip the
            %%    remaining steps. If the Contact header field is
            %%    present, the registrar checks if there is one
            %%    Contact field value that contains the special value
            %%    "*" and an Expires field.
            case ersip_sipmsg:find(expires, SipMsg) of
                not_found ->
                    %% an expiration time other than zero, the request is
                    %% invalid, and the server MUST return a 400 (Invalid Request) and
                    %% skip the remaining steps.
                    reply_bad_message(<<"No expires field">>, Request);
                {ok, {expires, V}} when V /= 0 ->
                    reply_bad_message(<<"Expires must be zero">>, Request);
                {ok, {expires, 0}} ->
                    Request1 = Request#request{action = remove_all_bindings},
                    next_phase(process_bindings, Request1)
            end;
        {ok, ContactHdrs} ->
            #request{config = Config} = Request,
            #config{options = #{default_expires := CfgExpires,
                                min_expires     := MinExpires
                               }} = Config,
            %% 7.
            ExpBindings = calculate_expires(ContactHdrs, SipMsg, CfgExpires),
            %% If and only if the requested expiration interval is greater
            %% than zero AND smaller than one hour AND less than a
            %% registrar-configured minimum, the registrar MAY reject the
            %% registration with a response of 423 (Interval Too Brief).  This
            %% response MUST contain a Min-Expires header field that states
            %% the minimum expiration interval the registrar is willing to
            %% honor.
            HasShortBinding = lists:any(fun({_, Exp}) ->
                                                Exp /= 0 andalso Exp < MinExpires
                                        end,
                                        ExpBindings),
            case HasShortBinding of
                true ->
                    reply_interval_too_brief(MinExpires, Request);
                false ->
                    Request1 = Request#request{action = {update_bindings, ExpBindings}},
                    next_phase(process_bindings, Request1)
            end
    end.

-spec process_bindings(event(), request()) -> request_result().
process_bindings(entry, #request{aoruri = AOR} = Request) ->
    {Request, ersip_registrar_se:find_bindings(AOR)};
process_bindings({lookup_result, {ok, SavedBindings}}, #request{action = request_all_bindings} = Request) ->
    Request1 = Request#request{result_bindings = SavedBindings},
    next_phase(prepare_answer, Request1);
process_bindings({lookup_result, {ok, SavedBindings}}, #request{action = remove_all_bindings, sipmsg = SipMsg} = Request) ->
    %% If not, the registrar checks whether the Call-ID agrees with
    %% the value stored for each binding.  If not, it MUST remove the
    %% binding.  If it does agree, it MUST remove the binding only if
    %% the CSeq in the request is higher than the value stored for
    %% that binding.  Otherwise, the update MUST be aborted and the
    %% request fails.
    CallId  = ersip_sipmsg:get(callid, SipMsg),
    CSeq    = ersip_sipmsg:get(cseq, SipMsg),
    CSeqVal = ersip_hdr_cseq:number(CSeq),
    case has_higher_cseq(CSeqVal, CallId, SavedBindings) of
        true ->
            reply_bad_message(<<"Invalid CSeq number">>, Request);
        false ->
            Event = #{added  => [],
                      removed => SavedBindings,
                      updated => []
                     },
            continue({update, Event}, set_phase(update_bindings, Request))
    end;
process_bindings({lookup_result, {ok, SavedBindings}}, #request{action = {update_bindings, ExpBindings}, sipmsg = SipMsg} = Request) ->
    CallId  = ersip_sipmsg:get(callid, SipMsg),
    CSeq    = ersip_sipmsg:get(cseq, SipMsg),
    CSeqVal = ersip_hdr_cseq:number(CSeq),
    case has_higher_cseq(CSeqVal, CallId, SavedBindings) of
        true ->
            %% If the Call-ID value in the existing binding differs
            %% from the Call-ID value in the request, the binding MUST
            %% be removed if the expiration time is zero and updated
            %% otherwise.  If they are the same, the registrar
            %% compares the CSeq value.  If the value is higher than
            %% that of the existing binding, it MUST update or remove
            %% the binding as above.  If not, the update MUST be
            %% aborted and the request fails.
            reply_bad_message(<<"Invalid CSeq number">>, Request);
        false ->
            %% For each address, the registrar then searches the list
            %% of current bindings using the URI comparison rules.  If
            %% the binding does not exist, it is tentatively added.
            NewBindings =
                [ersip_registrar_binding:new(CallId, CSeqVal, Contact, Exp)
                 || {Exp, Contact} <- ExpBindings,
                    binding_does_not_exist(Contact, SavedBindings),
                    Exp /= 0
                ],
            RemovedBindings =
                [Binding
                 || Binding <- SavedBindings,
                    binding_is_removed(Binding, ExpBindings)
                ],
            UpdatedBindings =
                [ersip_registrar_binding:update(Exp, CallId, CSeqVal, Binding)
                 || {Exp, Contact} <- ExpBindings,
                    Exp /= 0,
                    Binding <- find_saved_bindings(Contact, SavedBindings)
                ],
            Event =
                #{added   => NewBindings,
                  removed => RemovedBindings,
                  updated => UpdatedBindings
                 },
            continue({update, Event}, set_phase(update_bindings, Request))
    end;
process_bindings({lookup_result, {error, _}}, #request{sipmsg = SipMsg} = Request) ->
    ReplySipMsg = ersip_sipmsg:reply(500, SipMsg),
    terminate_request({reply, ReplySipMsg}, Request).


-spec update_bindings(event(), request()) -> request_result().
update_bindings({update, Event}, #request{aoruri = AOR} = Request) ->
    #{added := Added, updated := Updated, removed := Removed} = Event,
    Request1 = Request#request{result_bindings = Updated ++ Added},
    {Request1, ersip_registrar_se:update_bindings(AOR, {added, Added}, {updated, Updated}, {removed, Removed})};
update_bindings({update_result, ok}, #request{} = Request) ->
    next_phase(prepare_answer, Request);
update_bindings({update_result, {error, _}}, #request{sipmsg = SipMsg} = Request) ->
    ReplySipMsg = ersip_sipmsg:reply(500, SipMsg),
    terminate_request({reply, ReplySipMsg}, Request).

%% 8. The registrar returns a 200 (OK) response.  The response MUST
%%    contain Contact header field values enumerating all current
%%    bindings.  Each Contact value MUST feature an "expires"
%%    parameter indicating its expiration interval chosen by the
%%    registrar.
-spec prepare_answer(event(), request()) -> request_result().
prepare_answer(entry, #request{sipmsg = SipMsg, result_bindings = Bindings} = Request) ->
    ReplySipMsg0 = ersip_sipmsg:reply(200, SipMsg),
    ContactList = [ersip_registrar_binding:contact(Binding) || Binding <- Bindings],
    ReplySipMsg1 = ersip_sipmsg:set(contact, ContactList, ReplySipMsg0),
    %% TODO: The response SHOULD include a Date header field.
    terminate_request({reply, ReplySipMsg1}, Request).

%% The registrar now processes each contact address in the Contact
%% header field in turn.  For each address, it determines the
%% expiration interval as follows:
%%
%% -  If the field value has an "expires" parameter, that value
%%    MUST be taken as the requested expiration.
%%
%% -  If there is no such parameter, but the request has an
%%    Expires header field, that value MUST be taken as the
%%    requested expiration.
%%
%% -  If there is neither, a locally-configured default value MUST
%%    be taken as the requested expiration.
-spec calculate_expires([ersip_hdr_contact:contact()], ersip_sipmsg:sipmsg(), non_neg_integer()) -> [exp_binding()].
calculate_expires(NewBindings, SipMsg, CfgExpires) ->
    DefExpires =
        case ersip_sipmsg:find(expires, SipMsg) of
            not_found ->
                CfgExpires;
            {ok, {expires, V}} ->
                V
        end,
    lists:map(fun(Contact) ->
                      {ersip_hdr_contact:expires(Contact, DefExpires), Contact}
              end,
              NewBindings).

-spec terminate_request(ersip_registrar_se:side_effect(), request()) -> request_result().
terminate_request(Action, #request{} = Request) ->
    {set_phase(terminated, Request), Action}.


-spec next_phase(request_phase(), request()) -> request_result().
next_phase(Phase, Request) ->
    continue(entry, set_phase(Phase, Request)).

-spec reply_bad_message(Reason :: binary(), request()) -> request_result().
reply_bad_message(Reason, #request{config = #config{options = #{to_tag := ToTag}}, sipmsg = SipMsg} = Request) ->
    Reply = ersip_reply:new(400, [{reason, Reason},
                                  {to_tag, ToTag}
                                 ]),
    ReplySipMsg = ersip_sipmsg:reply(Reply, SipMsg),
    terminate_request({reply, ReplySipMsg}, Request).

-spec reply_interval_too_brief(MinExpires :: pos_integer(), request()) -> request_result().
reply_interval_too_brief(MinExpires, #request{config = #config{options = #{to_tag := ToTag}}, sipmsg = SipMsg} = Request) ->
    Reply = ersip_reply:new(423, [{to_tag, ToTag}]),
    ReplySipMsg0 = ersip_sipmsg:reply(Reply, SipMsg),
    ReplySipMsg1 = ersip_sipmsg:set(minexpires, {expires, MinExpires}, ReplySipMsg0),
    terminate_request({reply, ReplySipMsg1}, Request).

-spec has_higher_cseq(non_neg_integer(), ersip_hdr_callid:callid(), [ersip_registrar_binding:binding()]) -> boolean().
has_higher_cseq(CSeqVal, CallId, SavedBindings) ->
    lists:any(fun(SavedBinding) ->
                      {SavedCallId, SavedCSeqNum} = ersip_registrar_binding:callid_cseq(SavedBinding),
                      SavedCallId == CallId andalso SavedCSeqNum >= CSeqVal
              end,
              SavedBindings).

-spec binding_does_not_exist(ersip_hdr_contact:contact(), [ersip_registrar_binding:binding()]) -> boolean().
binding_does_not_exist(Contact, SavedBindings) ->
    ContactURI = ersip_hdr_contact:uri(Contact),
    ContactURIKey = ersip_uri:make_key(ContactURI),
    lists:all(fun(SavedBinding) ->
                      BindingContactURIKey = ersip_registrar_binding:contact_key(SavedBinding),
                      ContactURIKey /= ersip_uri:make_key(BindingContactURIKey)
              end,
              SavedBindings).

-spec binding_is_removed(ersip_registrar_binding:binding(), [{Exp :: non_neg_integer(), ersip_hdr_contact:contact()}]) -> boolean().
binding_is_removed(Binding, ExpBindings) ->
    RegisteredContactURIKey = ersip_registrar_binding:contact_key(Binding),
    lists:any(fun({0, ReqContact}) ->
                         ReqContactURI = ersip_hdr_contact:uri(ReqContact),
                         ReqContactKey = ersip_uri:make_key(ReqContactURI),
                         ReqContactKey == RegisteredContactURIKey;
                 ({_, _}) ->
                      false
              end,
              ExpBindings).

-spec find_saved_bindings(ersip_hdr_contact:contact(), [ersip_registrar_binding:binding()]) -> [ersip_registrar_binding:binding()].
find_saved_bindings(Contact, SavedBindings) ->
    ContactKey = ersip_uri:make_key(ersip_hdr_contact:uri(Contact)),
    [Binding || Binding <- SavedBindings,
                ersip_registrar_binding:contact_key(Binding) == ContactKey].
