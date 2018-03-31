%%
%% Copyright (c) 2018 Dmitry Poroh
%% All rights reserved.
%% Distributed under the terms of the MIT License. See the LICENSE file.
%%
%% SIP Message Source
%%
%%

-module(ersip_source).

-export([ new/3,
          is_tls/1 ]).
-export_type([ source/0 ]).

%%%===================================================================
%%% Types
%%%===================================================================

-record(source, { peer      :: { ersip_host:host(), inet:port_number() },
                  transport :: ersip_transport:transport(),
                  state     :: term()
                }).
-type source() :: #source{}.

%%%===================================================================
%%% API
%%%===================================================================

-spec new(Peer, ersip_transport:transport(), State) -> source() when
      Peer  :: { ersip_host:host(), inet:port_number() },
      State :: term().
new(Peer, Transport, State) ->
    #source{ peer = Peer,
             transport = Transport,
             state = State
           }.

-spec is_tls(source()) -> boolean().
is_tls(#source{ transport = T }) ->
    ersip_transport:is_tls(T).
