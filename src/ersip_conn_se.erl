%%
%% Copyright (c) 2018 Dmitry Poroh
%% All rights reserved.
%% Distributed under the terms of the MIT License. See the LICENSE file.
%%
%% SIP Connection Side effects
%%

-module(ersip_conn_se).

-export([ bad_message/2,
          new_message/1,
          disconnect/1
        ]).

-export_type([ side_effect/0 ]).

%%%===================================================================
%%% Types
%%%===================================================================

%% Bad datagram received.
-type side_effect() :: { bad_message, { error, term() }, binary() }
                     | { new_message, ersip_msg:message() }
                     | { disconnect, { error, term() } }.

%%%===================================================================
%%% API
%%%===================================================================

-spec bad_message({ error, term() }, binary() | ersip_msg:message()) -> side_effect().
bad_message(Error, Data) ->
    { bad_message, Error, Data }.

-spec new_message(ersip_msg:message()) -> { new_message, ersip_msg:message() }.
new_message(Message) ->
    { new_message, Message }.

-spec disconnect({ error, term() }) -> side_effect().
disconnect(Error) ->
    { disconnect, Error }.