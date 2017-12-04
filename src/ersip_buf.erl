%%
%% Copyright (c) 2017 Dmitry Poroh
%% All rights reserved.
%% Distributed under the terms of the MIT License. See the LICENSE file.
%%
%% SIP message buffer
%%

-module(ersip_buf).

-export([ new/1,
          add/2,
          read_till_crlf/1
        ]).

-type options() :: map().
-record(state,{ options = #{}          :: options(),
                acc     = []           :: list(binary()),
                head    = <<>>         :: binary(),
                queue   = queue:new()  :: queue:queue(binary())
              }).

-type state() :: #state{}.

-export_type([ state/0,
               options/0 
             ]).

-define(crlf, <<$\r, $\n>>).

%%%===================================================================
%%% API
%%%===================================================================

-spec new(options()) -> state().
new(Options) ->
    #state{ options = Options }.

%% @doc Put raw binary to the buffer.
-spec add(binary(), state()) -> state().
add(Binary, State) when is_binary(Binary) ->
    State#state{queue=queue:in(Binary, queue(State))}.

-spec read_till_crlf(state()) -> Result when
      Result :: { ok, binary(), state() }
              | { more_data, state() }.
read_till_crlf(State) ->
    { more_data, State }.

%%%===================================================================
%%% internal implementation
%%%===================================================================

-spec queue(state()) -> queue:queue(binary()).
queue(#state{ queue = Queue }) ->
    Queue.

-spec refill(state()) -> state().
refill(State) ->
    {{value,Item},Q} = queue:out(queue(State)),
    State#state{ head = Item,
                 queue = Q }.
    
