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

-include("ersip_headers.hrl").

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
-type int_parse_ret() :: {more_data, data()}
                       | {continue, data()}
                       | {{error, term()}, data()}
                       | {{ok, ersip_msg:message()}, data()}.

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
parse(#data{state = State} = Data) ->
    Result =
        case State of
            first_line -> parse_first_line(Data);
            headers    -> parse_headers(Data);
            body       -> parse_body(Data)
        end,
    case Result of
        {continue, Data1} -> parse(Data1);
        {more_data, _} = Ret -> Ret;
        {{error, _}, _} = Error -> Error;
        {{ok, _}, _}  = Ok -> Ok
    end.

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

-spec parse_first_line(data()) -> int_parse_ret().
parse_first_line(#data{buf = Buf} = Data) ->
    case ersip_buf:read_till_crlf(Buf) of
        {ok, Line, Buf_} ->
            Data_ = update(buf, Buf_, Data),
            parse_first_line(Line, Data_);
        {more_data, Buf_} ->
            Data_ = update(buf, Buf_, Data),
            more_data_required(Data_)
    end.

-spec parse_first_line(binary(), data()) -> int_parse_ret().
parse_first_line(<<"SIP/", _/binary>> = StatusLine, Data) ->
    parse_status_line(StatusLine, Data);
parse_first_line(<<>>, Data) ->
    %% Implementations processing SIP messages over stream-oriented
    %% transports MUST ignore any CRLF appearing before the start-line
    %% [H4.1].
    {continue, Data};
parse_first_line(RequestLine, Data) ->
    parse_request_line(RequestLine, Data).

-spec parse_status_line(binary(), data()) -> int_parse_ret().
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
            {continue, Data_};
        _ ->
            make_error({bad_status_line, StatusLine}, Data)
    end;
parse_status_line(StatusLine, Data) ->
    make_error({bad_status_line, StatusLine}, Data).

-spec parse_request_line(binary(), data()) -> int_parse_ret().
parse_request_line(RequestLine, #data{} =Data) when is_binary(RequestLine) ->
    Parsers = [fun ersip_method:parse/1,
               fun parse_sp/1,
               fun parse_ruri/1,
               fun parse_sp/1,
               fun parse_sip20/1],
    case ersip_parser_aux:parse_all(RequestLine, Parsers) of
        {ok, [Method, _, RURI, _, sip20], <<>>} ->
            Message  = ?message(Data),
            Message_ = ersip_msg:set([{type,   request},
                                      {method, Method },
                                      {ruri,   RURI   }],
                                     Message),
            Data_ = update([{message, Message_},
                            {state,   headers }],
                           Data),
            {continue, Data_};
        {ok, _, _} ->
            make_error({bad_message, {bad_request_line, RequestLine}}, Data);
        {error, Reason} ->
            make_error({bad_message, {bad_request_line, Reason}}, Data)
    end.


-spec parse_headers(data()) -> int_parse_ret().
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
                    {continue, DataA};
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
    Parsers = [fun parse_field_name/1,
               fun ersip_parser_aux:trim_lws/1,
               fun(Bin) -> ersip_parser_aux:parse_sep($:, Bin) end,
               fun ersip_parser_aux:trim_lws/1],
    case ersip_parser_aux:parse_all(H, Parsers) of
        {ok, [HName, _, _, _], V} ->
            Message_ = ersip_msg:add(HName, [V | Rest], ?message(Data)),
            {ok, update(message, Message_, Data)};
        _ ->
            {error, {bad_header,H}}
    end.

-spec parse_body(data()) -> int_parse_ret().
parse_body(#data{buf = Buf, message = Msg, content_len = undefined} = Data ) ->
    Hdr = ersip_msg:get(?ERSIPH_CONTENT_LENGTH, Msg),
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


%% field-name      =       1*ftext
%%
%% ftext           =       %d33-57 /               ; Any character except
%%                         %d59-126                ;  controls, SP, and
%%                                                 ;  ":".
-spec parse_field_name(binary()) -> ersip_parser_aux:parse_result(binary()).
parse_field_name(Bin) ->
    Pos = find_field_name_end(Bin, 0),
    case Pos of
        0 -> {error, {invalid_header_name, Bin}};
        _ ->
            <<FieldName:Pos/binary, Rest/binary>> = Bin,
           {ok, FieldName, Rest}
    end.

-spec find_field_name_end(binary(), non_neg_integer()) -> non_neg_integer().
find_field_name_end(<<C:8, Rest/binary>>, Pos)
  when (C >= 33 andalso C =< 57)
       orelse (C >= 59 andalso C =< 126) ->
    find_field_name_end(Rest, Pos+1);
find_field_name_end(_, Pos) ->
    Pos.


-spec parse_sp(binary()) -> ersip_parser_aux:parse_result(sp).
parse_sp(<<" ", Rest/binary>>) ->
    {ok, sp, Rest};
parse_sp(Bin) ->
    {error, {invalid_separator, Bin}}.

-spec parse_ruri(binary()) -> ersip_parser_aux:parse_result(ersip_method:method()).
parse_ruri(Bin) ->
    case find_ruri_end(Bin) of
        0 -> {error, {invalid_ruri, Bin}};
        Pos ->
            <<RURI:Pos/binary, Rest/binary>> = Bin,
            {ok, RURI, Rest}
    end.

-spec parse_sip20(binary()) -> ersip_parser_aux:parse_result(sip20).
parse_sip20(<<"SIP/2.0", Rest/binary>>) ->
    {ok, sip20, Rest};
parse_sip20(Bin) ->
    {error, {invalid_version, Bin}}.

-spec find_ruri_end(binary()) -> non_neg_integer().
find_ruri_end(Bin) ->
    case binary:match(Bin, <<" ">>) of
        {Pos, 1} -> Pos;
        nomatch -> byte_size(Bin)
    end.
