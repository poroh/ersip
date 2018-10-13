%%
%% Copyright (c) 2017, 2018 Dmitry Poroh
%% All rights reserved.
%% Distributed under the terms of the MIT License. See the LICENSE file.
%%
%% Highlevel (request line, headers, body)
%% SIP request/response parser
%%

-module(ersip_parser).

-export([new/0,
         new/1,
         new_dgram/1,
         add_binary/2,
         parse/1
        ]).

-export_type([data/0]).

%%%===================================================================
%%% Types
%%%===================================================================

-record(data, {options = #{}             :: map(),
               buf                       :: ersip_buf:state(),
               state = first_line        :: state(),
               message = ersip_msg:new() :: ersip:message(),
               acc   = []                :: list(binary()),
               content_len  = undefined  :: pos_integer() | undefined,
               %% Stream postion (ersip_buf:stream_postion/1) of the
               %% beginning of the message.
               start_pos = 0             :: non_neg_integer()
              }).
-type state()   :: first_line | headers | body.
-type data()    :: #data{}.
-type options() :: #{buffer => ersip_buf:options(),
                     max_message_len => unlimited | pos_integer()
                    }.
-type result()  :: more_data
                 | {error, term()}
                 | {ok, ersip:message()}.

%%%===================================================================
%%% API
%%%===================================================================

-spec new() -> data().
new() ->
    new(#{}).

-spec new(options()) -> data().
new(Options) ->
    #data{options = maps:merge(default_options(), Options),
          buf     = ersip_buf:new(maps:get(buffer, Options, #{}))
         }.

-spec new_dgram(binary()) -> data().
new_dgram(DatagramBinary) when is_binary(DatagramBinary) ->
    #data{buf = ersip_buf:new_dgram(DatagramBinary), options = #{max_message_len => unlimited}}.


-spec add_binary(binary(), data()) -> data().
add_binary(Binary, #data{buf=Buf} = Data) ->
    update(buf, ersip_buf:add(Binary, Buf), Data).

%% -spec parse(data()) -> {result(), data()}.
parse(#data{state = first_line} = Data) ->
    parse_first_line(Data);
parse(#data{state = headers} = Data) ->
    parse_headers(Data);
parse(#data{state = body} = Data) ->
    parse_body(Data).

%%%===================================================================
%%% internal implementation
%%%===================================================================

-define(message(Data), Data#data.message).

default_options() ->
    #{buffer => #{},
      max_message_len => 8192
     }.

-spec update(list({Item, Value}), data()) -> data() when
      Item :: buf
            | acc
            | state
            | content_len
            | message
            | start_pos,
      Value :: term().
update(List, Data) ->
    lists:foldl(fun({Item, Value}, Acc) ->
                        update(Item, Value, Acc)
                end,
                Data,
                List).

-spec update(buf,          ersip_buf:state(),     data()) -> data();
            (acc,          [binary()],            data()) -> data();
            (state,        state(),               data()) -> data();
            (content_len,  integer() | undefined, data()) -> data();
            (message,      ersip_msg:message(),   data()) -> data();
            (start_pos,    non_neg_integer(),     data()) -> data().
update(buf,         Buffer,  #data{} = Data) -> Data#data{buf         = Buffer};
update(acc,         Acc,     #data{} = Data) -> Data#data{acc         = Acc};
update(state,       State,   #data{} = Data) -> Data#data{state       = State};
update(content_len, Len,     #data{} = Data) -> Data#data{content_len = Len};
update(message,     Message, #data{} = Data) -> Data#data{message     = Message};
update(start_pos,   Pos,     #data{} = Data) -> Data#data{start_pos   = Pos}.

-spec parse_first_line(data()) -> {result(), data()}.
parse_first_line(#data{buf = Buf} = Data) ->
    case ersip_buf:read_till_crlf(Buf) of
        {ok, Line, Buf_} ->
            Data_ = update(buf, Buf_, Data),
            parse_first_line(Line, Data_);
        {more_data, Buf_} ->
            Data_ = update(buf, Buf_, Data),
            more_data_required(Data_)
    end.

-spec parse_first_line(binary(), data()) -> {result(), data()}.
parse_first_line(<<"SIP/", _/binary>> = StatusLine, Data) ->
    parse_status_line(StatusLine, Data);
parse_first_line(RequestLine, Data) ->
    parse_request_line(RequestLine, Data).

-spec parse_status_line(binary(), data()) -> {result(), data()}.
parse_status_line(<<"SIP/2.0 ", CodeBin:3/binary, " ", ReasonPhrase/binary>> = StatusLine, Data) ->
    case catch binary_to_integer(CodeBin) of
        Code when is_integer(Code) andalso Code >= 100 andalso Code =< 699 ->
            Message  = ?message(Data),
            Message_ = ersip_msg:set([{type,   response    },
                                      {status, Code        },
                                      {reason, ReasonPhrase}],
                                     Message),
            Data_ = update([{message, Message_},
                            {state,   headers }],
                           Data),
            parse(Data_);
        _ ->
            make_error({bad_status_line, StatusLine}, Data)
    end;
parse_status_line(StatusLine, Data) ->
    make_error({bad_status_line, StatusLine}, Data).

-spec parse_request_line(Arg, data()) -> {result(), data()} when
      Arg :: binary()
           | list(binary()).
parse_request_line(RequestLine, #data{} =Data) when is_binary(RequestLine) ->
    Splitted = binary:split(RequestLine, <<" ">>, [global]),
    parse_request_line(Splitted, Data);
parse_request_line([MethodBin, RURI, <<"SIP/2.0">>], #data{} = Data) when is_binary(MethodBin) ->
    case ersip_method:parse(MethodBin) of
        {ok, Method} ->
            parse_request_line([Method, RURI, <<"SIP/2.0">>], Data);
        {error, Reason} ->
            make_error({bad_message, Reason}, Data)
    end;
parse_request_line([Method, RURI, <<"SIP/2.0">>], #data{} = Data) ->
    Message  = ?message(Data),
    Message_ = ersip_msg:set([{type,   request},
                              {method, Method },
                              {ruri,   RURI   }],
                             Message),
    Data_ = update([{message, Message_},
                    {state,   headers }],
                   Data),
    parse(Data_);
parse_request_line(ReqLine, Data) ->
    make_error({bad_request_line, ReqLine}, Data).

-spec parse_headers(data()) -> {result(), data()}.
parse_headers(#data{acc = [], buf = Buf} = Data) ->
    case ersip_buf:read_till_crlf(Buf) of
        {more_data, Buf_} ->
            Data_ = update(buf, Buf_, Data),
            more_data_required(Data_);
        {ok, <<>>, Buf_} ->
            make_error({bad_message, no_headers}, update(buf, Buf_, Data));
        {ok, Line, Buf_} ->
            Data_ = update([{buf, Buf_},
                            {acc, [Line]}
                           ], Data),
            parse_headers(Data_)
    end;
parse_headers(#data{buf = Buf, acc = Acc} = Data) ->
    case ersip_buf:read_till_crlf(Buf) of
        {more_data, Buf_} ->
            Data_ = update(buf, Buf_, Data),
            more_data_required(Data_);
        {ok, <<FirstChar, _/binary>> = Cont, Buf_} when FirstChar == $  % LWS
                                                        orelse FirstChar == $\t ->
            Data_ = update([{buf, Buf_},
                            {acc, [Cont | Acc]}
                           ], Data),
            parse_headers(Data_);
        {ok, <<>>, Buf_} ->
            case add_header(lists:reverse(Acc), Data) of
                {ok, Data_}  ->
                    DataA = update([{state, body},
                                    {buf,   Buf_},
                                    {acc,   []  }
                                   ], Data_),
                    parse(DataA);
                {error, Reason} ->
                    Data_ = update(buf, Buf_, Data),
                    make_error(Reason, Data_)
            end;
        {ok, NewLine, Buf_} ->
            case add_header(lists:reverse(Acc), Data) of
                {ok, Data_} ->
                    DataA = update([{acc, [NewLine]},
                                    {buf, Buf_}
                                   ], Data_),
                    parse_headers(DataA);
                {error, Reason} ->
                    make_error(Reason, Data)
            end
    end.

-spec add_header(iolist(), data()) -> {ok, data()} | {error, term()}.
add_header([H|Rest], #data{} = Data) ->
    case binary:split(H, <<":">>) of
        [HName, V] ->
            Message_ = ersip_msg:add(ersip_bin:trim_lws(HName), [V | Rest], ?message(Data)),
            {ok, update(message, Message_, Data)};
        _ ->
            {error, {bad_header,H}}
    end.

-spec parse_body(data()) -> {result(), data()}.
parse_body(#data{buf = Buf, message = Msg, content_len = undefined} = Data ) ->
    Hdr = ersip_msg:get(<<"content-length">>, Msg),
    case ersip_hdr:as_integer(Hdr) of
        {ok, Value} when Value >= 0 ->
            parse_body(Data#data{content_len = Value});
        _ ->
            %% Applications SHOULD use this field to indicate the size of the
            %% message-body to be transferred, regardless of the media type of the
            %% entity.  If a stream-based protocol (such as TCP) is used as
            %% transport, the header field MUST be used.
            case ersip_buf:has_eof(Buf) of
                true ->
                    parse_body(Data#data{content_len = ersip_buf:length(Buf)});
                false ->
                    make_error({bad_message, {invalid_content_length, ersip_hdr:raw_values(Hdr)}}, Data)
            end
    end;
parse_body(#data{buf = Buf, content_len = Len, start_pos = StartPos} = Data) ->
    case ersip_buf:read(Len, Buf) of
        {more_data, Buf_} ->
            case ersip_buf:has_eof(Buf) of
                true ->
                    make_error({bad_message, <<"Truncated message">>}, Data);
                false ->
                    Data_ = update(buf, Buf_, Data),
                    more_data_required(Data_)
            end;
        {ok, Body, Buf_} ->
            StreamPos = ersip_buf:stream_postion(Buf_),
            Message = ersip_msg:set(body, Body, ?message(Data)),
            MessageLen = StreamPos - StartPos,
            Data_ = update([{message, ersip_msg:new()},
                            {content_len, undefined  },
                            {state, first_line},
                            {buf, Buf_},
                            {start_pos, StreamPos}
                           ], Data),
            message_parsed(Message, MessageLen, Data_)
    end.

-spec make_error(term(), data()) -> {{error, term()}, data()}.
make_error(Error, #data{} = Data) ->
    {{error, Error}, Data}.


-spec more_data_required(data()) ->  {more_data, data()} | {{error, message_too_long}, data()}.
more_data_required(#data{options = #{max_message_len := unlimited}} = Data) ->
    {more_data, Data};
more_data_required(#data{options = #{max_message_len := MaxLen}, start_pos = Start, buf = Buf} = Data) ->
    BufPos = ersip_buf:stream_postion(Buf),
    AlreadyRead = BufPos - Start,
    Accumulated = ersip_buf:length(Buf),
    case AlreadyRead + Accumulated of
        X when X > MaxLen ->
            make_error(message_too_long, Data);
        _ ->
            {more_data, Data}
    end.

-spec message_parsed(ersip:message(), MsgLen :: non_neg_integer(), data()) -> {{ok, ersip:message()}, data()} | {{error, message_too_long}, data()}.
message_parsed(Message, _MessageLen, #data{options = #{max_message_len := unlimited}} = Data) ->
    {{ok, Message}, Data};
message_parsed(_Message, MessageLen, #data{options = #{max_message_len := MaxLen}} = Data) when MessageLen > MaxLen ->
    make_error(message_too_long, Data);
message_parsed(Message, _MessageLen, #data{} = Data) ->
    {{ok, Message}, Data}.

