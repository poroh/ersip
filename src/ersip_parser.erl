%%
%% Copyright (c) 2017 Dmitry Poroh
%% All rights reserved.
%% Distributed under the terms of the MIT License. See the LICENSE file.
%%
%% Highlevel (request line, headers, body)
%% SIP request/response parser
%%

-module(ersip_parser).

-export([ new/0,
          new/1,
          add_binary/2,
          parse/1
        ]).

-record(data, { options = #{}      :: map(),
                buf                :: ersip_buf:state(),
                state = first_line :: state(),
                message            :: ersip:message(),
                acc   = []         :: list(binary())
               }).

-type state()   :: first_line | headers | body.
-type data()    :: #data{}.
-type options() :: #{ buffer => ersip_msg:options() }.
-type result()  :: more_data
                 | { error, term() }
                 | { ok, ersip:message() }.

%%%===================================================================
%%% API
%%%===================================================================

-spec new() -> data().
new() ->
    new(#{}).

-spec new(options()) -> data().
new(Options) ->
    #data{ options = Options,
           buf     = ersip_buf:new(maps:get(buffer, Options, #{})),
           message = ersip_msg:new()
         }.

-spec add_binary(binary(), data()) -> data().
add_binary(Binary, #data{buf=Buf} = Data) ->
    update(buf, ersip_buf:add(Binary, Buf), Data).

-spec parse(data()) -> { result(), data() }.
parse(#data{ state = first_line } = Data) ->
    parse_first_line(Data);
parse(#data{ state = headers } = Data) ->
    parse_headers(Data);
parse(#data{ state = body } = Data) ->
    parse_body(Data).

%%%===================================================================
%%% internal implementation
%%%===================================================================

-define(message(Data), Data#data.message).

-spec update(list({Item, Value}), data()) -> data() when
      Item :: buf
            | state
            | message,
      Value :: term().
update(List, Data) ->
    lists:foldl(fun({Item, Value}, Acc) ->
                        update(Item, Value, Acc)
                end,
                Data,
                List).

-spec update(buf,     ersip_buf:state(),   data()) -> data();
            (acc,     [ binary() ],        data()) -> data();
            (state,   state(),             data()) -> data();
            (message, ersip_msg:message(), data()) -> data().
update(buf,     Buffer,  #data{} = Data) -> Data#data{ buf = Buffer };
update(acc,     Acc,     #data{} = Data) -> Data#data{ acc = Acc };
update(state,   State,   #data{} = Data) -> Data#data{ state = State };
update(message, Message, #data{} = Data) -> Data#data{ message = Message }.

-spec parse_first_line(data()) -> { result(), data() }.
parse_first_line(#data{ buf = Buf } = Data) ->
    case ersip_buf:read_till_crlf(Buf) of
        { ok, Line, Buf_ } ->
            Data_ = update(buf, Buf_, Data),
            parse_first_line(Line, Data_);
        { more_data, Buf_ } ->
            Data_ = update(buf, Buf_, Data),
            { more_data, Data_ }
    end.

-spec parse_first_line(binary(), data()) -> { result(), data() }.
parse_first_line(<<"SIP/", _/binary>> = StatusLine, Data) ->
    parse_status_line(StatusLine, Data);
parse_first_line(RequestLine, Data) ->
    parse_request_line(RequestLine, Data).

-spec parse_status_line(binary(), data()) -> { result(), data() }.
parse_status_line(<<"SIP/2.0 ", CodeBin:3/binary, " ", ReasonPhrase/binary>> = StatusLine, Data) ->
    case catch binary_to_integer(CodeBin) of
        Code when is_integer(Code) andalso Code >= 100 andalso Code =< 699 ->
            Message  = ?message(Data),
            Message_ = ersip_msg:set([ { type,   response     },
                                       { status, Code         },
                                       { reason, ReasonPhrase } ],
                                     Message),
            Data_ = update([ { message, Message_ },
                             { state,   headers  } ],
                           Data),
            parse(Data_);
        _ ->
            make_error({bad_status_line, StatusLine}, Data)
    end;
parse_status_line(StatusLine, Data) ->
    make_error({bad_status_line, StatusLine}, Data).

-spec parse_request_line(Arg, data()) -> { result(), data() } when
      Arg :: binary()
           | list(binary()).
parse_request_line(RequestLine, Data) when is_binary(RequestLine) ->
    Splitted = binary:split(RequestLine, <<" ">>, [global]),
    parse_request_line(Splitted, Data);
parse_request_line([ Method, RURI, <<"SIP/2.0">>], Data) ->
    Message  = ?message(Data),
    Message_ = ersip_msg:set([ { type,   request },
                               { method, Method  },
                               { ruri,   RURI    } ],
                             Message),
    Data_ = update([ { message, Message_ },
                     { state,   headers  } ],
                   Data),
    parse(Data_);
parse_request_line(ReqLine, Data) ->
    make_error({bad_request_line, ReqLine}, Data).

-spec parse_headers(data()) -> { result(), data() }.
parse_headers(#data{ acc = [], buf = Buf } = Data) ->
    case ersip_buf:read_till_crlf(Buf) of
        { more_data, Buf_ } ->
            { more_data, update(buf, Buf_, Data) };
        { ok, <<>>, Buf_ } ->
            make_error({bad_message, no_headers}, update(buf, Buf_, Data));
        { ok, Line, Buf_ } ->
            Data_ = update([ { buf, Buf_ },
                             { acc, [ Line ] }
                           ], Data),
            parse_headers(Data_)
    end;
parse_headers(#data{ buf = Buf, acc = Acc } = Data) ->
    case ersip_buf:read_till_crlf(Buf) of
        { more_data, Buf_ } ->
            { more_data, update(buf, Buf_, Data) };
        { ok, <<FirstChar, _/binary>> = Cont, Buf_ } when FirstChar =:= $  % LWS
                                                   orelse FirstChar =:= $\t ->
            Data_ = update([ { buf, Buf_ },
                             { acc, [ Cont | Acc ] }
                           ], Data),
            parse_headers(Data_);
        { ok, <<>>, Buf_ } ->
            case add_header(lists:reverse(Acc), Data) of
                { ok, Data_ }  ->
                    DataA = update([ { state, body },
                                     { buf,   Buf_ },
                                     { acc,   []   }
                                   ], Data_),
                    parse(DataA);
                { error, Reason } ->
                    Data_ = update(buf, Buf_, Data),
                    make_error(Reason, Data_)
            end;
        { ok, NewLine, Buf_ } ->
            case add_header(lists:reverse(Acc), Data) of
                { ok, Data_ } ->
                    DataA = update([ { acc, [ NewLine ] },
                                     { buf, Buf_ }
                                   ], Data_),
                    parse_headers(DataA);
                { error, Reason } ->
                    make_error(Reason, Data)
            end
    end.

-spec add_header(iolist(), data()) -> { ok, data() } | { error, term() }.
add_header([H|Rest], Data) ->
    case binary:split(H, <<":">>) of
        [ HName, V ] ->
            Message_ = ersip_msg:add(HName, [ V | Rest ], ?message(Data)),
            { ok, update(message, Message_, Data) };
        _ ->
            { error, {bad_header,H} }
    end.

-spec parse_body(data()) -> { result(), data() }.
parse_body(Data) ->
    make_error(not_implemented_yet, Data).

-spec make_error(term(), data()) -> { { error, term() }, data() }.
make_error(Error, Data) ->
    { {error, Error}, Data }.