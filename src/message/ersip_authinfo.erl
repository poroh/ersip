%%
%% Copyright (c) 2018 Dmitry Poroh
%% All rights reserved.
%% Distributed under the terms of the MIT License. See the LICENSE file.
%%
%% Authoriztion info
%% Includes: common challenge and common credentials
%%

-module(ersip_authinfo).

-export([make/1,
         parse/1,
         assemble/1,
         assemble_bin/1]).

%%%===================================================================
%%% Types
%%%===================================================================

-record(authinfo, {type   :: binary(),
                   params :: ersip_parser_aux:gen_param_list()
                  }).
-type authinfo() :: #authinfo{}.

%%%===================================================================
%%% API
%%%===================================================================

-spec make(binary()) -> authinfo().
make(Bin) ->
    case parse(Bin) of
        {ok, A} -> A;
        {error, Reason} ->
            error({parse_error, Reason})
    end.

-spec parse(binary()) -> ersip_parser_aux:parse_result(authinfo()).
parse(Bin) ->
    Parsers = [fun ersip_parser_aux:parse_token/1,
               fun ersip_parser_aux:trim_lws/1,
               fun parse_params/1
              ],
    case ersip_parser_aux:parse_all(Bin, Parsers) of
        {ok, [AuthType, _, ParamsList], <<>>} ->
            {ok,
             #authinfo{type   = AuthType,
                       params = ParamsList
                    }
            };
        {ok, _, Rest} ->
            {error, {invalid_authinfo, {garbage_at_the_end, Rest}}};
        {error, Reason} ->
            {error, {invalid_authinfo, Reason}}
    end.

-spec assemble(authinfo()) -> iolist().
assemble(#authinfo{type = T, params = P}) ->
    [T, <<" ">>,
     ersip_iolist:join(<<", ">>,
                       [[K, <<"=">>, V] || {K, V} <- P])].

-spec assemble_bin(authinfo()) -> binary().
assemble_bin(#authinfo{} = A) ->
    iolist_to_binary(assemble(A)).

%%%===================================================================
%%% Internals
%%%===================================================================

-spec parse_params(binary()) -> ersip_parser_aux:parse_result(ersip_parser_aux:gen_param_list()).
parse_params(Binary) ->
    ersip_parser_aux:parse_params($,, Binary).

