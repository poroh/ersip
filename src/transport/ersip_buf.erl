%%
%% Copyright (c) 2017 Dmitry Poroh
%% All rights reserved.
%% Distributed under the terms of the MIT License. See the LICENSE file.
%%
%% SIP message buffer
%%

-module(ersip_buf).

-export([new/1,
         new_dgram/1,
         add/2,
         length/1,
         stream_postion/1,
         set_eof/1,
         has_eof/1,
         read_till_crlf/1,
         read/2
        ]).
-export_type([state/0,
              options/0
             ]).

%%%===================================================================
%%% Types
%%%===================================================================

-type options() :: map().
-record(state,{options  = #{}          :: options(),
               acc      = <<>>         :: binary() | iolist(),
               acclen   = 0            :: non_neg_integer(),
               queue    = queue:new()  :: queue:queue(binary()),
               queuelen = 0            :: non_neg_integer(),
               eof      = false        :: boolean(),
               %% Current position in the buffer (beginning of acc).
               pos      = 0            :: non_neg_integer(),
               crlf     = binary:compile_pattern(<<"\r\n">>)
              }).

-type state() :: #state{}.

-define(queue(State), State#state.queue).
-define(queuelen(State), State#state.queuelen).
-define(CRLF_LEN, 2).

%%%===================================================================
%%% API
%%%===================================================================

%% @doc New buffer with specified options.
%% Note options are reserved in API for future use.
-spec new(options()) -> state().
new(Options) ->
    #state{options = Options}.

%% @doc New buffer with datagram.
-spec new_dgram(binary()) -> state().
new_dgram(Binary) ->
    S0 = new(#{}),
    S1 = add(Binary, S0),
    set_eof(S1).

%% @doc Put raw binary to the buffer.
-spec add(binary(), state()) -> state().
add(Binary, #state{eof = false, queuelen = Len} = State) when is_binary(Binary) ->
    Q =    queue:in(Binary, ?queue(State)),
    QLen = Len + byte_size(Binary),
    State#state{queue    = Q,
                queuelen = QLen
               }.

%% @doc Add eof to the buffer
-spec set_eof(state()) -> state().
set_eof(State) ->
    State#state{eof=true}.

%% @doc Buffer has EOF
-spec has_eof(state()) -> boolean().
has_eof(#state{eof=EOF}) ->
    EOF.

%% @doc number of bytes accumulated inside the buffer.
-spec length(state()) -> non_neg_integer().
length(#state{acclen = AccLen, queuelen = QLen}) ->
    QLen + AccLen.

%% @doc Current position in the stream (number of stream bytes read
%% out from this buffer).
-spec stream_postion(state()) -> non_neg_integer().
stream_postion(#state{pos = Pos}) ->
    Pos.

%% @doc Reads buffer until CRLF is found.  CRLF is not included in
%% output and skipped on next read.
-spec read_till_crlf(state()) -> Result when
      Result :: {ok, binary(), state()}
              | {more_data, state()}.
read_till_crlf(#state{acc = A, crlf = CRLF} = State) ->
    do_read_till_crlf(CRLF, byte_size(A)-1, State).

%% @doc Read number of bytes from the buffer.
-spec read(Len :: pos_integer(), state()) -> Result when
      Result :: {ok, iolist(), state()}
              | {more_data, state()}.
read(Len, #state{acclen = AccLen} = State) when Len >= AccLen->
    read_more_to_acc(Len-AccLen, State).

%%%===================================================================
%%% internal implementation
%%%===================================================================

%% @doc Read from buffer till Patter. Position is starting search
%% position in accamulator. May be optimized in part of Acc
%% concatenation.
%%
%% @private
-spec do_read_till_crlf(Pattern, Position, state()) -> Result when
      Pattern  :: binary() | binary:cp(),
      Position :: integer(),
      Result :: {ok, binary(), state()}
              | {more_data, state()}.
do_read_till_crlf(Pattern, Pos, State) when Pos < 0 ->
    do_read_till_crlf(Pattern, 0, State);
do_read_till_crlf(Pattern, Pos, #state{acc = A} = State) ->
    Queue = ?queue(State),
    QueueLen = ?queuelen(State),
    case binary:match(A, Pattern, [{scope, {Pos, byte_size(A) - Pos}}]) of
        nomatch ->
            case queue:is_empty(Queue) of
                true ->
                    {more_data, State};
                false ->
                    {{value, Item}, Q} = queue:out(Queue),
                    QLen = QueueLen - byte_size(Item),
                    Acc_ = <<A/binary, Item/binary>>,
                    Pos_ = byte_size(A) - ?CRLF_LEN + 1,
                    State_ = State#state{queue    = Q,
                                         queuelen = QLen,
                                         acc      = Acc_,
                                         acclen   = byte_size(Acc_)
                                        },
                    do_read_till_crlf(Pattern, Pos_, State_)
            end;
        {Start, Len} ->
            RestPos = Start + Len,
            RestLen = byte_size(A) - RestPos,
            Q =
                case RestLen of
                    0 ->
                        Queue;
                    _ ->
                        queue:in_r(binary:part(A, RestPos, RestLen), Queue)
                end,
            QLen = QueueLen + RestLen,
            NewBufPos = State#state.pos + RestPos,
            State_ = State#state{queue    = Q,
                                 queuelen = QLen,
                                 acc      = <<>>,
                                 acclen   = 0,
                                 pos      = NewBufPos
                                },
            {ok, binary:part(A, 0, Start), State_}
    end.

-spec read_more_to_acc(Len :: non_neg_integer(), state()) -> Result when
      Result :: {ok, iolist(), state()}
              | {more_data, state()}.
read_more_to_acc(Len, #state{acc = <<>>} = State) ->
    read_more_to_acc(Len, State#state{acc = []});
read_more_to_acc(0, #state{acc = Acc, acclen = AccLen, pos = Pos} = State) ->
    State_ = State#state{acc = <<>>,
                         acclen = 0,
                         pos = Pos + AccLen
                        },
    {ok, lists:reverse(Acc), State_};
read_more_to_acc(Len, #state{acc = Acc, acclen = AccLen} = State) ->
    Queue = ?queue(State),
    QueueLen = ?queuelen(State),
    case queue:out(Queue) of
        {empty, Queue} ->
            {more_data, State};
        {{value, V}, Q} ->
            Size = byte_size(V),
            QLen = QueueLen - Size,
            case Size of
                Sz when Sz =< Len ->
                    TotalAcc = AccLen + Sz,
                    State1 = State#state{acc = [V | Acc],
                                         acclen = TotalAcc,
                                         queue = Q,
                                         queuelen = QLen
                                        },
                    read_more_to_acc(Len-Sz, State1);
                Sz when Sz > Len ->
                    HPart = binary:part(V, 0, Len),
                    RPart = binary:part(V, Len, Sz-Len),
                    Q1    = queue:in_r(RPart, Q),
                    Q1Len = QLen + byte_size(RPart),
                    State1 = State#state{acc = lists:reverse([HPart | Acc]),
                                         acclen = AccLen + Len,
                                         queue = Q1,
                                         queuelen = Q1Len
                                        },
                    read_more_to_acc(0, State1)
            end
    end.

