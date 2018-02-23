%%
%% Copyright (c) 2017 Dmitry Poroh
%% All rights reserved.
%% Distributed under the terms of the MIT License. See the LICENSE file.
%%
%% SIP message buffer
%%

-module(ersip_buf).

-export([ new/1,
          new_dgram/1,
          add/2,
          length/1,
          set_eof/1,
          has_eof/1,
          read_till_crlf/1,
          read/2
        ]).

-type options() :: map().
-record(state,{ options = #{}          :: options(),
                acc     = <<>>         :: binary() | iolist(),
                acclen  = 0            :: non_neg_integer(),
                queue   = queue:new()  :: queue:queue(binary()),
                eof     = false        :: boolean()
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

%% @doc New buffer with datagram.
-spec new_dgram(binary()) -> state().
new_dgram(Binary) ->
    S0 = new(#{}),
    S1 = add(Binary, S0),
    set_eof(S1).

%% @doc Put raw binary to the buffer.
-spec add(binary(), state()) -> state().
add(Binary, #state{ eof = false } = State) when is_binary(Binary) ->
    State#state{queue=queue:in(Binary, ?queue(State))}.

%% @doc Add eof to the buffer
-spec set_eof(state()) -> state().
set_eof(State) ->
    State#state{eof=true}.

%% @doc Buffer has EOF
-spec has_eof(state()) -> boolean().
has_eof(#state{eof=EOF}) ->
    EOF.

-spec length(state()) -> non_neg_integer().
length(#state{ acclen = 0, acc = <<>>, eof = true } = State) -> 
    calc_length(?queue(State), 0).

%% @doc Reads buffer until CRLF is found.  CRLF is not included in
%% output and skipped on next read.
-spec read_till_crlf(state()) -> Result when
      Result :: { ok, binary(), state() }
              | { more_data, state() }.
read_till_crlf(#state{ acc = A } = State) ->
    read_till(?crlf, byte_size(A)-1, State).

%% @doc Read number of bytes from the buffer.
-spec read(Len :: pos_integer(), state()) -> Result when
      Result :: { ok, iolist(), state() }
              | { more_data, state() }.
read(Len, #state{ acclen = AccLen } = State) when Len >= AccLen->
    read_more_to_acc(Len-AccLen, State).

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
                    read_till(Pattern, Pos_, State#state{ queue = Q, acc = Acc_, acclen = byte_size(Acc_) })
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
            { ok, binary:part(A, 0, Start), State#state{ queue = Q, acc = <<>>, acclen = 0 } }
    end.

-spec read_more_to_acc(Len :: non_neg_integer(), state()) -> Result when
      Result :: { ok, iolist(), state() }
              | { more_data, state() }.
read_more_to_acc(Len, #state{ acc = <<>> } = State) ->
    read_more_to_acc(Len, State#state{ acc = [] });
read_more_to_acc(0, #state{ acc = Acc } = State) ->
    { ok, lists:reverse(Acc), State#state{ acc = <<>>, acclen = 0 } };
read_more_to_acc(Len, #state{ acc = Acc, acclen = AccLen } = State) ->
    Queue = ?queue(State),
    case queue:out(Queue) of
        {empty, Queue} ->
            { more_data, State };
        {{value, V}, Q} ->
            case byte_size(V) of
                Sz when Sz =< Len ->
                    TotalAcc = AccLen + Sz,
                    State1 = State#state{ acc = [ V | Acc ],
                                          acclen = TotalAcc,
                                          queue = Q },
                    read_more_to_acc(Len-Sz, State1);
                Sz when Sz > Len ->
                    HPart = binary:part(V, 0, Len),
                    RPart = binary:part(V, Len, Sz-Len),
                    Q1    = queue:in_r(RPart, Q),
                    State1 = State#state{ acc = lists:reverse([ HPart | Acc ]),
                                          acclen = AccLen + Sz,
                                          queue = Q1 },
                    read_more_to_acc(0, State1)
            end
    end.

calc_length(Q, Acc) ->
    case queue:out(Q) of
        {empty, Q} ->
            Acc;
        {{value, V}, Q1} ->
            calc_length(Q1, Acc + byte_size(V))
    end.
