%%
%% Copyright (c) 2018 Dmitry Poroh
%% All rights reserved.
%% Distributed under the terms of the MIT License. See the LICENSE file.
%%
%% Side effects of stateful proxy
%%

-module(ersip_proxy_se).

-export([create_trans/3,
         delete_trans/1,
         response/2,
         select_target/1,
         set_timer/2,
         stop/0]).

-export_type([side_effect/0,
              timer_event/0]).

%%%===================================================================
%%% Types
%%%===================================================================

-type side_effect() :: create_trans()
                     | response()
                     | select_target()
                     | set_timer()
                     | stop().

-type create_trans()  :: {create_trans, {Type :: trans_type(),  Id :: term(), ersip_request:request() | ersip_sipmsg:sipmsg()}}.
-type delete_trans()  :: {delete_trans, Id :: term()}.
-type response()      :: {response, {Id :: term(), ersip_sipmsg:sipmsg()}}.
-type select_target() :: {select_target, RURI :: ersip_uri:uri()}.
-type set_timer()     :: {set_timer, {timeout(), timer_event()}}.
-type stop()          :: {stop, {}}.

-type trans_type()  :: client | server.
-type timer_event() :: {timer, ersip_branch:branch_key(), reference()}
                     | {cancel_timer, ersip_branch:branch_key()}.

%%%===================================================================
%%% API
%%%===================================================================

%% @doc Create transaction side effect.
%%
%% On this event transaction of specified type need to be created.
%% Note ersip_trans:new_server/2 and ersip_trans:new_client/2.
%%
%% Once transaction return some result then ersip_proxy:trans_result
%% must be called with:
%% 1. For new SIP message: ersip_proxy:trans_result(Id, SipMsg, State)
%% 2. For timeout: ersip_proxy:trans_result(Id, timeout, State)
-spec create_trans(trans_type(), term(), ersip_request:request() | ersip_sipmsg:sipmsg()) -> create_trans().
create_trans(TransType, Id, Req) ->
    {create_trans, {TransType, Id, Req}}.


%% @doc Force delete transaction. This side effect is generated when
%% INVITE transaction does not respond, so timer C is fired and we
%% need to terminate previously created transaction to prevent
%% resource leak.
-spec delete_trans(term()) -> delete_trans().
delete_trans(Id) ->
    {delete_trans, Id}.

%% @doc Send response within transaction with identifier Id.
-spec response(Id :: term(), ersip_sipmsg:sipmsg()) -> response().
response(Id, SipMsg) ->
    {response, {Id, SipMsg}}.

%% @doc Select target side effect.
%%
%% On this side effect proxy need to select destination of the
%% request. Once destination is selected ersip_proxy:forward_to/2 must
%% be called.
-spec select_target(ersip_uri:uri()) -> select_target().
select_target(RURI) ->
    {select_target, RURI}.

%% @doc Set timer side effect.
%%
%% On this side effect proxy need to set timer. Once this timer fired
%% ersip_proxy:timer_fired/2 must be called.
-spec set_timer(timeout(), timer_event()) -> set_timer().
set_timer(Timeout, TimerEv) ->
    {set_timer, {Timeout, TimerEv}}.

%% @doc Stop side effect.
%%
%% State may be safely cleared after stop side effect has got.
-spec stop() -> stop().
stop() ->
    {stop, {}}.
