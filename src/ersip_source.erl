%%
%% Copyright (c) 2018 Dmitry Poroh
%% All rights reserved.
%% Distributed under the terms of the MIT License. See the LICENSE file.
%%
%% SIP Message Source
%%
%%

-module(ersip_source).

-export([new/3,
         source_id/1,
         is_tls/1]).
-export_type([source/0]).

%%%===================================================================
%%% Types
%%%===================================================================

-record(source, {peer      :: {ersip_host:host(), inet:port_number()},
                 transport :: ersip_transport:transport(),
                 source_id :: term()
                }).
-type source() :: #source{}.

%%%===================================================================
%%% API
%%%===================================================================

-spec new(Peer, ersip_transport:transport(), SourceId) -> source() when
      Peer  :: {ersip_host:host(), inet:port_number()},
      SourceId :: term().
new(Peer, Transport, SourceId) ->
    #source{peer = Peer,
            transport = Transport,
            source_id = SourceId
           }.

-spec source_id(source()) -> term().
source_id(#source{source_id = SId}) ->
    SId.

-spec is_tls(source()) -> boolean().
is_tls(#source{transport = T}) ->
    ersip_transport:is_tls(T).
