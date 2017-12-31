%%
%% Copyright (c) 2017 Dmitry Poroh
%% All rights reserved.
%% Distributed under the terms of the MIT License. See the LICENSE file.
%%
%% Binary routines helpers
%%

-module(ersip_bin).

-export([to_lower/1, trim_head_lws/1 ]).

%%%===================================================================
%%% API
%%%===================================================================

-spec to_lower( binary() ) -> binary().
to_lower(Binary) when is_binary(Binary) ->
    << <<(unicode_to_lower(C))/utf8>> || <<C/utf8>> <= Binary >>;
to_lower(Binary) -> erlang:error(badarg, [Binary]).

-spec trim_head_lws( binary() ) -> binary().
trim_head_lws(<<>>) ->
    <<>>;
trim_head_lws(<<$ , Rest/binary>>) ->
    trim_head_lws(Rest);
trim_head_lws(<<$\t, Rest/binary>>) ->
    trim_head_lws(Rest);
trim_head_lws(Binary) when is_binary(Binary) ->
    Binary.

%%%===================================================================
%%% internal implementation
%%%===================================================================

-spec unicode_to_lower( byte() ) -> byte().
unicode_to_lower(C) ->
    hd(string:to_lower([C])).

