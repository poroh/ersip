%%
%% Copyright (c) 2018 Dmitry Poroh
%% All rights reserved.
%% Distributed under the terms of the MIT License. See the LICENSE file.
%%
%% SIP Message Source
%%
%%

-module(ersip_source).

-export([new/4,
         local/1,
         remote/1,
         source_id/1,
         set_source_id/2,
         transport/1,
         is_tls/1]).
-export_type([source/0]).

%%%===================================================================
%%% Types
%%%===================================================================

-record(source, {local     :: {ersip_host:host(), inet:port_number()},
                 peer      :: {ersip_host:host(), inet:port_number()},
                 transport :: ersip_transport:transport(),
                 source_id :: term()
                }).
-type source() :: #source{}.

%%%===================================================================
%%% API
%%%===================================================================

-spec new(Local, Peer, ersip_transport:transport(), SourceId) -> source() when
      Local  :: {ersip_host:host(), inet:port_number()},
      Peer   :: {ersip_host:host(), inet:port_number()},
      SourceId :: term().
new(Local, Peer, Transport, SourceId) ->
    #source{local     = Local,
            peer      = Peer,
            transport = Transport,
            source_id = SourceId
           }.


-spec local(source()) -> {ersip_host:host(), inet:port_number()}.
local(#source{local = Local}) ->
    Local.

-spec remote(source()) -> {ersip_host:host(), inet:port_number()}.
remote(#source{peer = Remote}) ->
    Remote.

-spec source_id(source()) -> term().
source_id(#source{source_id = SId}) ->
    SId.

-spec set_source_id(term(), source()) -> source().
set_source_id(SID, #source{} = Source) ->
    Source#source{source_id = SID}.

-spec transport(source()) -> ersip_transport:transport().
transport(#source{transport = T}) ->
    T.

-spec is_tls(source()) -> boolean().
is_tls(#source{transport = T}) ->
    ersip_transport:is_tls(T).
