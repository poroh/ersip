%%
%% Copyright (c) 2017 Dmitry Poroh
%% All rights reserved.
%% Distributed under the terms of the MIT License. See the LICENSE file.
%%
%% Binary routines helpers
%%

-module(ersip_bin).

-export([to_lower/1]).

%%%===================================================================
%%% API
%%%===================================================================

-spec to_lower( binary() ) -> binary().
to_lower(Binary) when is_binary(Binary) ->
    << <<(unicode_to_lower(C))/utf8>> || <<C/utf8>> <= Binary >>;
to_lower(Binary) -> erlang:error(badarg, [Binary]).

%%%===================================================================
%%% internal implementation
%%%===================================================================

-spec unicode_to_lower( char() ) -> char().
unicode_to_lower(C) ->
    hd(string:to_lower([C])).

