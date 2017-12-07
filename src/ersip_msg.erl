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
         add/3
        ]).

-type method() :: binary().
-record(message, { type    = undefined :: { request,  method() | undefined, binary() | undefined }
                                        | { response, 100..699 | undefined, binary() | undefined }
                                        | undefined,
                   headers = #{}       :: #{ binary() := list({ term(), iolist() } | iolist()) },
                   body    = []        :: iolist()
                 }).

-type item() :: type
              | status
              | reason
              | method
              | ruri.

-type message() :: #message{}.
-export_type([message/0, item/0 ]).

%%%===================================================================
%%% API
%%%===================================================================

-spec new() -> message().
new() ->
    #message{}.

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
set(ruri,   RURI,   #message{ type = { request,  X, _ } } = Message) -> Message#message{ type = { request,       X,    RURI } }.

-spec add(Name, Value, message()) -> message() when
      Name  :: binary(),
      Value :: iolist().
add(Header, Value, #message{ headers = H } = Message) ->
    LowerName = ersip_bin:to_lower(Header),
    Current = maps:get(LowerName, H, []),
    Message#message{ headers = H#{ LowerName => Current ++ [ Value ] } }.

%%%===================================================================
%%% internal implementation
%%%===================================================================
