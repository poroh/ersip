%%
%% Copyright (c) 2017 Dmitry Poroh
%% All rights reserved.
%% Distributed under the terms of the MIT License. See the LICENSE file.
%%
%% SIP message
%%

-module(ersip_msg).

-export([new/0,
         set/2,
         set/3,
         add/3,
         get/2,
         serialize/1
        ]).

-type method() :: binary().
-record(message, { type    = undefined :: { request,  method() | undefined, binary() | undefined }
                                        | { response, 100..699 | undefined, binary() | undefined }
                                        | undefined,
                   headers = #{}       :: #{ binary() := list({ term(), iolist() } | iolist()) },
                   body    = []        :: iolist()
                 }).

-type header_name() :: binary().
-type item() :: type
              | status
              | reason
              | method
              | ruri
              | body
              | header_name().

-type message() :: #message{}.
-export_type([message/0, item/0 ]).

%%%===================================================================
%%% API
%%%===================================================================

-spec new() -> message().
new() ->
    #message{}.

-spec get(item(),     message()) -> term();
         ([ item() ], message()) -> [ term() ].
get(ItemList, Message) when is_list(ItemList) ->
    lists:map(fun(Item) ->
                      { Item, get(Item, Message) }
              end,
              ItemList);
get(type,   #message{ type = { X, _, _ } })        -> X;
get(status, #message{ type = { response, X, _ } }) -> X;
get(reason, #message{ type = { response, _, X } }) -> X;
get(method, #message{ type = { request,  X, _ } }) -> X;
get(ruri,   #message{ type = { request,  _, X } }) -> X;
get(body,   #message{ body = X                  }) -> X;
get(HeaderName, #message{ headers = H }) when is_binary(HeaderName) ->
    Key = ersip_hdr:make_key(HeaderName),
    maps:get(Key, H, ersip_hdr:new(HeaderName)).

-spec set(list({item(), term()}), message()) -> message().
set(List, Message) ->
    lists:foldl(fun({Item, Value}, Acc) ->
                        set(Item, Value, Acc)
                end,
                Message,
                List).

-spec set(item(), term(), message()) -> message().
set(type, _X, #message{ type = { _X, _, _ } } = Message) ->
    Message;
set(type, X, Message) ->
    Message#message{ type = { X, undefined, undefined } };
set(status, Status, #message{ type = { response, _, X } } = Message) -> Message#message{ type = { response, Status,       X } };
set(reason, Reason, #message{ type = { response, X, _ } } = Message) -> Message#message{ type = { response,      X,  Reason } };
set(method, Method, #message{ type = { request,  _, X } } = Message) -> Message#message{ type = { request,  Method,       X } };
set(ruri,   RURI,   #message{ type = { request,  X, _ } } = Message) -> Message#message{ type = { request,       X,    RURI } };
set(body,   Body,   Message) -> Message#message{ body = Body }.

-spec add(Name, Value, message()) -> message() when
      Name  :: binary(),
      Value :: iolist().
add(HeaderName, Value, #message{ headers = H } = Message) ->
    case ersip_iolist:trim_head_lws(Value) of
        [] ->
            Message;
        V ->
            Key = ersip_hdr:make_key(HeaderName),
            Current = maps:get(Key, H, ersip_hdr:new(HeaderName)),
            Updated = ersip_hdr:add_value(V, Current),
            Message#message{ headers = H#{ Key => Updated } }
    end.

-spec serialize(message()) -> iolist().
serialize(#message{} = Message) ->
    L = lists:foldl(fun(F, Acc) -> F(Message, Acc) end,
                    [],
                    [ fun serialize_first_line/2,
                      fun serialize_headers/2,
                      fun serialize_body/2
                    ]),
    lists:reverse(L).


%%%===================================================================
%%% internal implementation
%%%===================================================================

-spec serialize_first_line(message(), iolist()) -> iolist().
serialize_first_line(#message{ type={request, Method, RURI} }, Acc) ->
    [ <<" SIP/2.0">>, RURI, <<" ">>, Method | Acc ];
serialize_first_line(#message{ type={response, StatusCode, Reason} }, Acc) ->
    [ Reason, <<" ">>, StatusCode, <<"SIP/2.0 ">> | Acc ].

-spec serialize_headers(message(), iolist()) -> iolist().
serialize_headers(#message{ headers = Headers }, Acc) ->
    Acc.

serialize_body(#message{ body = Body }, Acc) ->
    [ Body | Acc ].
