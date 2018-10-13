%%
%% Copyright (c) 2018 Dmitry Poroh
%% All rights reserved.
%% Distributed under the terms of the MIT License. See the LICENSE file.
%%
%% SIP Connection Side effects
%%

-module(ersip_conn_se).

-export([bad_message/2,
         new_request/1,
         new_response/2,
         disconnect/1
        ]).

-export_type([side_effect/0]).

%%%===================================================================
%%% Types
%%%===================================================================

%% Bad datagram received.
-type side_effect() :: bad_message()
                     | new_request()
                     | new_response()
                     | disconnect().
-type bad_message()  :: {bad_message, Reason :: term(), binary() | ersip_msg:message()}.
-type new_request()  :: {new_request, ersip_msg:message()}.
-type new_response() :: {new_response, ersip_hdr_via:via(), ersip_msg:message()}.
-type disconnect()   :: {disconnect, {error, term()}}.

%%%===================================================================
%%% API
%%%===================================================================

-spec bad_message(binary() | ersip_msg:message(), Reason :: term()) -> bad_message().
bad_message(Data, Error) ->
    {bad_message, Data, Error}.

-spec new_request(ersip_msg:message()) -> new_request().
new_request(Message) ->
    {new_request, Message}.

-spec new_response(ersip_hdr_via:via(), ersip_msg:message()) -> new_response().
new_response(Via, Message) ->
    {new_response, Via, Message}.

-spec disconnect({error, term()}) -> disconnect().
disconnect(Error) ->
    {disconnect, Error}.
