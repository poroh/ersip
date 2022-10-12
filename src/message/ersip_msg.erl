%%
%% Copyright (c) 2017 Dmitry Poroh
%% All rights reserved.
%% Distributed under the terms of the MIT License. See the LICENSE file.
%%
%% Raw SIP message
%%

-module(ersip_msg).

-export([new/0,
         set/2,
         set/3,
         set_header/2,
         del_header/2,
         add/3,
         get/2,
         header_keys/1,
         get_headers/1,
         clear_headers/1,
         source/1,
         set_source/2,
         serialize/1,
         serialize_bin/1
        ]).
-export_type([message/0,
              item/0,
              type/0
             ]).

-include("ersip_headers.hrl").

%%%===================================================================
%%% Types
%%%===================================================================

-type method() :: ersip_method:method().
-record(message, {type    = undefined :: {request,  method() | undefined, binary() | undefined}
                                       | {response, 100..699 | undefined, binary() | undefined}
                                       | undefined,
                  headers = #{}       :: #{ersip_hnames:header_key() := ersip_hdr:header()},
                  body    = []        :: iolist(),
                  source = undefined  :: undefined
                                       | ersip_source:source()
                 }).

-type header_name() :: ersip_hnames:name_forms().
-type item() :: type
              | status
              | reason
              | method
              | ruri
              | body
              | header_name().

-type message() :: #message{}.
-type type()    :: request | response.

%%%===================================================================
%%% API
%%%===================================================================

-spec new() -> message().
new() ->
    #message{}.

-spec get(item(),     message()) -> term();
         ([item()], message()) -> [term()].
get(ItemList, Message) when is_list(ItemList) ->
    lists:map(fun(Item) ->
                      {Item, get(Item, Message)}
              end,
              ItemList);
get(type,   #message{type = {X, _, _}})        -> X;
get(status, #message{type = {response, X, _}}) -> X;
get(reason, #message{type = {response, _, X}}) -> X;
get(method, #message{type = {request,  X, _}}) -> X;
get(ruri,   #message{type = {request,  _, X}}) -> X;
get(body,   #message{body = X                 }) -> X;
get({hdr_key, _} = Key, #message{headers = H}) ->
    case H of
        #{Key := Hdr} -> Hdr;
        _ ->
            ersip_hdr:new(Key)
    end;
get(HeaderName, #message{headers = H}) ->
    NewHdr = ersip_hdr:new(HeaderName),
    Key    = ersip_hdr:make_key(NewHdr),
    maps:get(Key, H, NewHdr).

-spec header_keys(message()) -> [ersip_hnames:header_key()].
header_keys(#message{headers = H}) ->
    maps:keys(H).

-spec get_headers(message()) -> [ersip_hdr:header()].
get_headers(#message{headers = H}) ->
    maps:values(H).

-spec clear_headers(message()) -> message().
clear_headers(#message{} = Message) ->
    Message#message{headers = #{}}.

-spec set(list({item(), term()}), message()) -> message().
set(List, Message) ->
    lists:foldl(fun({Item, Value}, Acc) ->
                        set(Item, Value, Acc)
                end,
                Message,
                List).

%% @doc Set message part to specified value.
-spec set(item(), term(), message()) -> message().
set(type, X, #message{type = {X, _, _}} = Message) ->
    Message;
set(type, X, Message) ->
    Message#message{type = {X, undefined, undefined}};
set(status, Status, #message{type = {response, _, X}} = Message) ->
    Message#message{type = {response, Status,       X}};
set(reason, Reason, #message{type = {response, X, _}} = Message) ->
    Message#message{type = {response,      X,  Reason}};
set(method, Method, #message{type = {request,  _, X}} = Message) ->
    case Method of
        {method, _} ->
            Message#message{type = {request,  Method,       X}};
        MethodBin when is_binary(MethodBin) ->
            Message#message{type = {request,  ersip_method:make(Method), X}};
        _ ->
            error({invalid_method_type, Method})
    end;
set(ruri,   RURI,   #message{type = {request,  X, _}} = Message) -> Message#message{type = {request,       X,    RURI}};
set(body,   Body,   Message) -> Message#message{body = Body};
set(HeaderName, {mval, Values},  #message{headers = H} = Message) ->
    Hdr = ersip_hdr:new(HeaderName),
    Key = ersip_hdr:make_key(Hdr),
    Message1 = Message#message{headers = H#{Key => Hdr}},
    lists:foldl(fun(Value, Msg) ->
                        add(HeaderName, Value, Msg)
                end,
                Message1,
                Values);
set(HeaderName, Value,  #message{headers = H} = Message) ->
    Header0 = ersip_hdr:new(HeaderName),
    Key = ersip_hdr:make_key(Header0),
    Header1 = ersip_hdr:add_value(Value, Header0),
    Message#message{headers = H#{Key => Header1}}.

-spec set_header(ersip_hdr:header(), message()) -> message().
set_header(Header, #message{headers = H} = Message) ->
    Key = ersip_hdr:make_key(Header),
    Message#message{headers = H#{Key => Header}}.

-spec del_header(ersip_hnames:name_forms() | ersip_hdr:header(), message()) -> message().
del_header(HeaderName, #message{headers = H} = Message) ->
    Key = ersip_hdr:make_key(HeaderName),
    Message#message{headers = maps:remove(Key, H)}.

-spec add(Name :: binary(), Value :: ersip_hdr:value(), message()) -> message().
add(HeaderName, Value, #message{headers = H} = Message) ->
    Key = ersip_hnames:make_key(HeaderName),
    case H of
        #{Key := Hdr} ->
            Updated = ersip_hdr:add_value(Value, Hdr),
            Message#message{headers = H#{Key => Updated}};
        #{} ->
            Hdr0 = ersip_hdr:new(HeaderName, Key),
            Hdr = ersip_hdr:add_value(Value, Hdr0),
            Message#message{headers = H#{Key => Hdr}}
    end.

-spec source(message()) -> undefined | ersip_source:source().
source(#message{source = Source}) ->
    Source.

-spec set_source(ersip_source:source()|undefined, message()) -> message().
set_source(Source, #message{} = Message) ->
    Message#message{source = Source}.

-spec serialize_bin(message()) -> binary().
serialize_bin(#message{} = Message) ->
    iolist_to_binary(serialize(Message)).

-spec serialize(message()) -> iolist().
serialize(#message{} = Message) ->
    L = lists:foldl(fun(F, Acc) -> F(Message, Acc) end,
                    [],
                    [fun serialize_first_line/2,
                     fun serialize_headers/2,
                     fun serialize_body/2
                    ]),
    lists:reverse(L).


%%%===================================================================
%%% internal implementation
%%%===================================================================

-define(headers_order, [<<"via">>, <<"to">>, <<"from">>,
                        <<"call-id">>,  <<"cseq">>, <<"max-forwards">>]).

-spec serialize_first_line(message(), iolist()) -> iolist().
serialize_first_line(#message{type={request, Method, RURI}}, Acc) ->
    [<<" SIP/2.0">>, RURI, <<" ">>, ersip_method:to_binary(Method) | Acc];
serialize_first_line(#message{type={response, StatusCode, Reason}}, Acc) ->
    [Reason, <<" ">>, integer_to_binary(StatusCode), <<"SIP/2.0 ">> | Acc].

-spec serialize_headers(message(), iolist()) -> iolist().
serialize_headers(#message{headers = Headers}, Acc) ->
    Ordered    = header_keys_order(),
    NotOrdered = maps:keys(maps:without([?ERSIPH_CONTENT_LENGTH | Ordered], Headers)),
    Acc1 = serialize_headers_in_order(Ordered, Headers, Acc),
    serialize_headers_in_order(NotOrdered, Headers, Acc1).

-spec serialize_body(message(), iolist()) -> iolist().
serialize_body(#message{body = Body}, Acc) ->
    case ersip_iolist:is_empty(Body) of
        true ->
            [<<"\r\n\r\n">>, <<"\r\nContent-Length: 0">> | Acc];
        false ->
            BodyBin = iolist_to_binary(Body),
            Size = integer_to_binary(byte_size(BodyBin)),
            [BodyBin, <<"\r\n\r\n">>, Size, <<"\r\nContent-Length: ">> | Acc]
    end.

header_keys_order() ->
    lists:map(fun ersip_hdr:make_key/1, ?headers_order).

serialize_headers_in_order([], _Headers, Acc) ->
    Acc;
serialize_headers_in_order([K|Rest], Headers, Acc) ->
    case Headers of
        #{K := Hdr} ->
            serialize_headers_in_order(Rest, Headers, ersip_hdr:serialize_rev_iolist(Hdr, Acc));
        _ ->
            serialize_headers_in_order(Rest, Headers, Acc)
    end.
