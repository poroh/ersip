%%
%% Copyright (c) 2018 Dmitry Poroh
%% All rights reserved.
%% Distributed under the terms of the MIT License. See the LICENSE file.
%%
%% SIP Connection Side effects
%%

-module(ersip_conn_se).

-export([ bad_datagram/2
        ]).

-export_type([ side_effect/0 ]).

%%%===================================================================
%%% Types
%%%===================================================================

%% Bad datagram received.
-type side_effect() :: { bad_datagram, { error, term() }, binary() }.

%%%===================================================================
%%% API
%%%===================================================================

-spec bad_datagram({ error, term() }, binary()) -> side_effect().
bad_datagram(Error, Data) ->
    { bad_datagram, Error, Data }.
    
