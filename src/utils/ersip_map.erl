%%%
%%% Copyright (c) 2020 Dmitry Poroh
%%% All rights reserved.
%%% Distributed under the terms of the MIT License. See the LICENSE file.
%%%
%%% IO list routines
%%%

-module(ersip_map).

-export([apply_to/3]).

%%===================================================================
%% Types
%%===================================================================

-type apply_pair() :: {Key :: atom(), apply_fun()}.
-type apply_fun()  :: fun((term(), tuple()) -> tuple()).

%%===================================================================
%% API
%%===================================================================

%% @doc Apply functions to target if key is memeber of SrcMap.
-spec apply_to([apply_pair()], map(), any()) -> any().
apply_to(Appliers, SrcMap, Target)
  when is_list(Appliers), is_map(SrcMap) ->
    lists:foldl(fun({Key, ApplyF}, T) ->
                        case SrcMap of
                            #{Key := Value} -> ApplyF(Value, T);
                            _ -> T
                        end
                end,
                Target,
                Appliers).

