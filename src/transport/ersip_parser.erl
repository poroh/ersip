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
    Data#data{buf = ersip_buf:add(Binary, Buf)}.

%% -spec parse(data()) -> {result(), data()}.
parse(#data{state = first_line, buf = Buf} = Data) ->
    case ersip_buf:read_till_crlf(Buf) of
        {ok, Line, Buf1} ->
            Data1 = Data#data{buf = Buf1},
            case parse_first_line(Line) of
                again -> parse(Data1);
                {ok, Message} ->
                    Data2 = Data1#data{message = Message,
                                       state = headers},
                    parse(Data2);
                {error, _} = Error ->
                    {Error, Data1}
            end;
        {more_data, Buf1} ->
            Data1 = Data#data{buf = Buf1},
            more_data_required(Data1)
    end;
parse(#data{state = headers, buf = Buf, acc = []} = Data) ->
    %% This is the first header of the message.
    case ersip_buf:read_till_crlf(Buf) of
        {ok, <<>>, Buf1} ->
            %% If acc is empty then no headers are specified in
            %% message
            Error = {error, {bad_message, no_headers}},
            Data1 = Data#data{buf = Buf1},
            {Error, Data1};
        {ok, Line, Buf1} ->
            %% Just remember header in acc and process input:
            Data1 = Data#data{buf = Buf1, acc = [Line]},
            parse(Data1);
        {more_data, Buf1} ->
            Data1 = Data#data{buf = Buf1},
            more_data_required(Data1)
    end;
parse(#data{state = headers, buf = Buf, acc = Acc, message = Msg} = Data) ->
    case ersip_buf:read_till_crlf(Buf) of
        {ok, <<>>, Buf1} ->
            %% Add accumulated header and go to body parsing
            Data1 = Data#data{buf = Buf1},
            case add_header(lists:reverse(Acc), Msg) of
                {ok, Msg1} ->
                    Data2 = Data1#data{acc = []},
                    case calc_content_length(Msg1, Buf1) of
                        {ok, Len} ->
                            Data3 = Data2#data{state = body,
                                               message = Msg1,
                                               content_len = Len},
                            parse(Data3);
                        {error, _} = Error ->
                            {Error, Data2}
                    end;
                {error, _} = Error ->
                    {Error, Data1}
            end;
        {ok, <<$ , _/binary>> = Cont, Buf1} ->
            %% Line beginning with LWS is continuation of headers,
            %% just add to accumulator
            Data1 = Data#data{buf = Buf1, acc = [Cont | Acc]},
            parse(Data1);
        {ok, <<$\t, _/binary>> = Cont, Buf1} ->
            Data1 = Data#data{buf = Buf1, acc = [Cont | Acc]},
            parse(Data1);
        {ok, Line, Buf1} ->
            %% Start of the new header - add previous to message and
            %% start accumulating new one
            Data1 = Data#data{buf = Buf1},
            case add_header(lists:reverse(Acc), Msg) of
                {ok, Msg1} ->
                    Data2 = Data1#data{acc = [Line], message = Msg1},
                    parse(Data2);
                {error, _} = Error ->
                    {Error, Data1}
            end;
        {more_data, Buf1} ->
            Data1 = Data#data{buf = Buf1},
            more_data_required(Data1)
    end;
parse(#data{state = body, content_len = Len, buf = Buf} = Data) ->
    case ersip_buf:read(Len, Buf) of
        {more_data, Buf1} ->
            case ersip_buf:has_eof(Buf) of
                true ->
                    make_error({bad_message, <<"Truncated message">>}, Data);
                false ->
                    Data1 = Data#data{buf = Buf1},
                    more_data_required(Data1)
            end;
        {ok, Body, Buf1} ->
            Msg       = Data#data.message,
            Msg1      = ersip_msg:set(body, Body, Msg),

            StartPos  = Data#data.start_pos,
            StreamPos = ersip_buf:stream_postion(Buf1),
            MsgLen = StreamPos - StartPos,

            %% Prepare state for next message
            Data1 = Data#data{message      = ersip_msg:new(),
                              content_len  = undefined,
                              state        = first_line,
                              buf          = Buf1,
                              start_pos     = StreamPos},
            message_parsed(Msg1, MsgLen, Data1)
    end.


%%%===================================================================
%%% internal implementation
%%%===================================================================

default_options() ->
    #{buffer => #{},
      max_message_len => 8192
     }.

-spec parse_first_line(binary()) -> {ok, ersip_msg:message()} | {error, term()} | again.
parse_first_line(<<"SIP/", _/binary>> = StatusLine) ->
    parse_status_line(StatusLine);
parse_first_line(<<>>) ->
    %% Implementations processing SIP messages over stream-oriented
    %% transports MUST ignore any CRLF appearing before the start-line
    %% [H4.1].
    again;
parse_first_line(RequestLine) ->
    parse_request_line(RequestLine).

-spec parse_status_line(binary()) -> {ok, ersip_msg:message()} | {error, term()}.
parse_status_line(<<"SIP/2.0 ", CodeBin:3/binary, " ", ReasonPhrase/binary>> = StatusLine) ->
    case catch binary_to_integer(CodeBin) of
        Code when is_integer(Code) andalso Code >= 100 andalso Code =< 699 ->
            Message0  = ersip_msg:new(),
            Message = ersip_msg:set([{type,   response    },
                                     {status, Code        },
                                     {reason, ReasonPhrase}],
                                    Message0),
            {ok, Message};
        _ ->
            {error, {bad_status_line, StatusLine}}
    end;
parse_status_line(StatusLine) ->
    {error, {bad_status_line, StatusLine}}.

-spec parse_request_line(binary()) -> {ok, ersip_msg:message()} | {error, term()}.
parse_request_line(RequestLine) when is_binary(RequestLine) ->
    Parsers = [fun ersip_method:parse/1,
               fun parse_sp/1,
               fun parse_ruri/1,
               fun parse_sp/1,
               fun parse_sip20/1],
    case ersip_parser_aux:parse_all(RequestLine, Parsers) of
        {ok, [Method, _, RURI, _, sip20], <<>>} ->
            Message0 = ersip_msg:new(),
            Message  = ersip_msg:set([{type,   request},
                                      {method, Method },
                                      {ruri,   RURI   }],
                                     Message0),
            {ok, Message};
        {ok, _, _} ->
            {error, {bad_message, {bad_request_line, RequestLine}}};
        {error, Reason} ->
            {error, {bad_message, {bad_request_line, Reason}}}
    end.

-spec add_header(iolist(), ersip_msg:message()) -> {ok, ersip_msg:message()} | {error, term()}.
add_header([H], Msg) ->
    Parsers = [fun parse_field_name/1,
               fun ersip_parser_aux:trim_lws/1,
               fun(Bin) -> ersip_parser_aux:parse_sep($:, Bin) end,
               fun ersip_parser_aux:trim_lws/1],
    case ersip_parser_aux:parse_all(H, Parsers) of
        {ok, [_, _, _, _], <<>>} ->
            {ok, Msg};
        {ok, [HName, _, _, _], V} ->
            Msg1 = ersip_msg:add(HName, V, Msg),
            {ok, Msg1};
        _ ->
            {error, {bad_header,H}}
    end;
add_header([H|Rest], Msg) ->
    Parsers = [fun parse_field_name/1,
               fun ersip_parser_aux:trim_lws/1,
               fun(Bin) -> ersip_parser_aux:parse_sep($:, Bin) end,
               fun ersip_parser_aux:trim_lws/1],
    case ersip_parser_aux:parse_all(H, Parsers) of
        {ok, [HName, _, _, _], <<>>} ->
            Bin = ersip_bin:trim_head_lws(iolist_to_binary(Rest)),
            Msg1 = ersip_msg:add(HName, Bin, Msg),
            {ok, Msg1};
        {ok, [HName, _, _, _], V} ->
            Msg1 = ersip_msg:add(HName, iolist_to_binary([V | Rest]), Msg),
            {ok, Msg1};
        _ ->
            {error, {bad_header,H}}
    end.

-spec calc_content_length(ersip_msg:message(), ersip_buf:state()) -> {ok, non_neg_integer()} | {error, term()}.
calc_content_length(Msg, Buf) ->
    Hdr = ersip_msg:get(?ERSIPH_CONTENT_LENGTH, Msg),
    case ersip_hdr:as_integer(Hdr) of
        {ok, Value} when Value >= 0 ->
            {ok, Value};
        _ ->
            %% Applications SHOULD use this field to indicate the size of the
            %% message-body to be transferred, regardless of the media type of the
            %% entity.  If a stream-based protocol (such as TCP) is used as
            %% transport, the header field MUST be used.
            case ersip_buf:has_eof(Buf) of
                true ->
                    {ok, ersip_buf:length(Buf)};
                false ->
                    {error, {bad_message, {invalid_content_length, ersip_hdr:raw_values(Hdr)}}}
            end
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
