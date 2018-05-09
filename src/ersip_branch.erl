%%
%% Copyright (c) 2018 Dmitry Poroh
%% All rights reserved.
%% Distributed under the terms of the MIT License. See the LICENSE file.
%%
%% SIP branch
%%

-module(ersip_branch).

-export([make/1,
         make_rfc3261/1,
         make_random/1,
         make_key/1,
         assemble/1,
         is_rfc3261/1
        ]).

%%%===================================================================
%%% Types
%%%===================================================================

-type branch()     :: {branch, binary()}.
-type branch_key() :: {branch_key, binary()}.

-export_type([branch/0,
              branch_key/0
             ]).
%%%===================================================================
%%% API
%%%===================================================================

-spec make(binary()) -> branch().
make(Bin) ->
    {branch, Bin}.

-spec make_rfc3261(binary()) -> branch().
make_rfc3261(Bin) ->
    case is_rfc3261({branch, Bin}) of
        true ->
            {branch, Bin};
        false ->
            {branch, <<"z9hG4bK", Bin/binary>>}
    end.

-spec make_random(NumBytes :: pos_integer()) -> branch().
make_random(NumBytes) ->
    make_rfc3261(ersip_id:token(crypto:strong_rand_bytes(NumBytes))).

%% @doc Create comparable key for branch parameter.
-spec make_key(branch()) -> branch_key().
make_key({branch, Bin}) ->
    {branch_key, ersip_bin:to_lower(Bin)};
make_key({branch_key, _} = Key) ->
    Key.

-spec assemble(branch()) -> iolist().
assemble({branch, Bin}) ->
    Bin.

-spec is_rfc3261(branch() | branch_key()) -> boolean().
is_rfc3261({branch, <<"z9hG4bK", _/binary>>}) ->
    true;
is_rfc3261({branch_key, <<"z9hg4bk", _/binary>>}) ->
    true;
is_rfc3261({branch, _}) ->
    false;
is_rfc3261({branch_key, _}) ->
    false.

