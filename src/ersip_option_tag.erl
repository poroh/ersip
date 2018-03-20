%%
%% Copyright (c) 2018 Dmitry Poroh
%% All rights reserved.
%% Distributed under the terms of the MIT License. See the LICENSE file.
%%
%% SIP Option-Tag
%% For example: 100rel, timers etc.
%%

-module(ersip_option_tag).

-export([ parse/1,
          make/1,
          to_binary/1
        ]).
-export_type([ option_tag/0 ]).

%%%===================================================================
%%% Types
%%%===================================================================

-type option_tag() :: { option_tag, binary() }.

%%%===================================================================
%%% API
%%%===================================================================

-spec parse(binary()) ->  { ok, option_tag() }
                        | { error, Error } when
      Error :: { invalid_option_tag, binary() }.
parse(Bin) ->
    case ersip_parser_aux:check_token(Bin) of
        true ->
            { ok, { option_tag, Bin } };
        false ->
            { error, { invalid_option_tag, Bin } }
    end.

-spec to_binary(option_tag()) -> binary().
to_binary({ option_tag, Bin }) ->
    Bin.

-spec make(binary()) -> option_tag(). 
make(Bin) ->
    case parse(Bin) of
        { ok, M } ->
            M;
        { error, _ } = Error ->
            error(Error)
    end.
