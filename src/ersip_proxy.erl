%%
%% Copyright (c) 2018 Dmitry Poroh
%% All rights reserved.
%% Distributed under the terms of the MIT License. See the LICENSE file.
%%
%% SIP proxy support
%%

-module(ersip_proxy).

-export_type([params/0]).

%%%===================================================================
%%% Types
%%%===================================================================

-type params() :: ersip_proxy_common:proxy_params().

%%%===================================================================
%%% API
%%%===================================================================
