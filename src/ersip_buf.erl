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
                acc     = <<>>         :: binary(),
                queue   = queue:new()  :: queue:queue(binary())
              }).

-type state() :: #state{}.

-export_type([ state/0,
               options/0
             ]).
-define(crlf, <<$\r,$\n>>).
-define(queue(State), State#state.queue).

%%%===================================================================
%%% API
%%%===================================================================

%% @doc New buffer with specified options.
%% Note options are reserved in API for future use.
-spec new(options()) -> state().
new(Options) ->
    #state{ options = Options }.

%% @doc Put raw binary to the buffer.
-spec add(binary(), state()) -> state().
add(Binary, State) when is_binary(Binary) ->
    State#state{queue=queue:in(Binary, ?queue(State))}.

%% @doc Reads buffer until CRLF is found.  CRLF is not included in
%% output and skipped on next read.
-spec read_till_crlf(state()) -> Result when
      Result :: { ok, binary(), state() }
              | { more_data, state() }.
read_till_crlf(#state{ acc = A } = State) ->
    read_till(?crlf, byte_size(A)-1, State).

%%%===================================================================
%%% internal implementation
%%%===================================================================

%% @doc Read from buffer till Patter. Position is starting search
%% position in accamulator. May be optimized in part of Acc
%% concatenation.
%%
%% @private
-spec read_till(Pattern, Position, state()) -> Result when
      Pattern  :: binary(),
      Position :: integer(),
      Result :: { ok, binary(), state() }
              | { more_data, state() }.
read_till(Pattern, Pos, State) when Pos < 0 ->
    read_till(Pattern, 0, State);
read_till(Pattern, Pos, #state{ acc = A } = State) ->
    Queue = ?queue(State),
    case binary:match(A, Pattern, [ { scope, {Pos, byte_size(A) - Pos }} ]) of
        nomatch ->
            case queue:is_empty(Queue) of
                true ->
                    { more_data, State };
                false ->
                    {{value, Item}, Q} = queue:out(Queue),
                    Acc_ = <<A/binary, Item/binary>>,
                    Pos_ = byte_size(A) - byte_size(Pattern) + 1,
                    read_till(Pattern, Pos_, State#state{ queue = Q, acc = Acc_ })
            end;
        { Start, Len } ->
            RestPos = Start + Len,
            RestLen = byte_size(A) - RestPos,
            Q =
                case RestLen of
                    0 ->
                        Queue;
                    _ ->
                        queue:in_r(binary:part(A, RestPos, RestLen), Queue)
                end,
            { ok, binary:part(A, 0, Start), State#state{ queue = Q, acc = <<>> } }
    end.
