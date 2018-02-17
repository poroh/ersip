%%
%% Copyright (c) 2018 Dmitry Poroh
%% All rights reserved.
%% Distributed under the terms of the MIT License. See the LICENSE file.
%%
%% SIP method
%%

-module(ersip_method).

-export([ parse/1,
          make/1,
          to_binary/1
        ]).
-export_type([ method/0 ]).

%%%===================================================================
%%% Types
%%%===================================================================

-type method() :: { method, binary() }.

%%%===================================================================
%%% API
%%%===================================================================

-spec parse(binary()) ->  { ok, method() }
                        | { error, term() }.
parse(Bin) ->
    case ersip_parser_aux:check_token(Bin) of
        true ->
            { ok, { method, Bin } };
        false ->
            { error, { invalid_method, Bin } }
    end.

-spec to_binary(method()) -> binary().
to_binary({ method, Bin }) ->
    Bin.

-spec make(binary()) -> method(). 
make(Bin) ->
    case parse(Bin) of
        { ok, M } ->
            M;
        { error, _ } = Error ->
            error(Error)
    end.
