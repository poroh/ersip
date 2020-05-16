%%%
%%% Copyright (c) 2018, 2020 Dmitry Poroh
%%% All rights reserved.
%%% Distributed under the terms of the MIT License. See the LICENSE file.
%%%
%%% @doc Authoriztion info.
%%% This is common module to process single value of following headers:
%%% - WWW-Authenticate
%%% - Authorization
%%% - Proxy-Authenticate
%%% - Proxy-Authorization
%%%
%%% Note that more than one Authorization header maybe present is SIP
%%% header.
%%% @see ersip_hdr_auth. for more information
%%%

-module(ersip_authinfo).

-export([type/1,
         raw_param/2,
         make/1,
         parse/1,
         assemble/1,
         assemble_bin/1,
         raw/1]).

-export_type([authinfo/0, raw/0]).

%%===================================================================
%% Types
%%===================================================================

-record(authinfo, {type   :: binary(),
                   params :: ersip_parser_aux:gen_param_list()
                  }).
-type authinfo() :: #authinfo{}.
-type raw() :: {Type :: binary(), [{binary(), binary()}]}.
-type parse_result() :: {ok, authinfo()} | {error, parse_error()}.
-type parse_error()  :: {invalid_authinfo, {garbage_at_the_end, binary()}}
                      | {invalid_authinfo, term()}.

%%===================================================================
%% API
%%===================================================================

%% @doc Type of the authorization info in lower case.
%% Example:
%% ```
%%   AuthInfo = ersip_authinfo:make(<<"Digest realm=\"testrealm@host.com\", qop=\"auth,auth-int\", nonce=\"dcd98b7102dd2f0e8b11d0f600bfb0c093\"">>),
%%   <<"digest">> = ersip_authinfo:type(AuthInfo).
%% '''
-spec type(authinfo()) -> binary().
type(#authinfo{type = T}) ->
    ersip_bin:to_lower(T).

%% @doc Get raw value of the parameter from authorization.
%% Note that value is returned as-is. If it was quoted string it is returned as quoted string.
%% If parameter is not found then undefined atom is returned.
%% Example:
%% ```
%%   AuthInfo = ersip_authinfo:make(<<"Digest realm=\"testrealm@host.com\", qop=\"auth,auth-int\", nonce=\"dcd98b7102dd2f0e8b11d0f600bfb0c093\"">>),
%%   {ok, <<"\"auth,auth-int\"">>} = ersip_authinfo:raw_param(<<"QOP">>, AuthInfo).
%%   undefined = ersip_authinfo:raw_param(<<"cnonce">>, AuthInfo).
%% '''
-spec raw_param(binary(), authinfo()) -> {ok, binary()} | undefined.
raw_param(Name, #authinfo{params = ParamList}) ->
    LName = ersip_bin:to_lower(Name),
    AllValues = [Value|| {Key, Value} <- ParamList, ersip_bin:to_lower(Key) == LName],
    case AllValues of
        [V | _] -> {ok, V};
        [] -> undefined
    end.

%% @doc Create AuthInfo from binary or from raw value.
%% If parameter is not wellformed header than function raises error.
%% Example:
%% ```
%%   AuthInfo = ersip_authinfo:make(<<"Digest realm=\"testrealm@host.com\", qop=\"auth,auth-int\", nonce=dcd98b7102dd2f0e8b11d0f600bfb0c093">>),
%%   AuthInfo = ersip_authinfo:make({<<"Digest">>, [{<<"realm">>, <<"testrealm@host.com">>},
%%                                                  {<<"qop">>, <<"auth,auth-int">>},
%%                                                  {<<"nonce">>, <<"dcd98b7102dd2f0e8b11d0f600bfb0c093">>}]}).
%% '''
-spec make(binary() | raw()) -> authinfo().
make(Bin) when is_binary(Bin) ->
    case parse(Bin) of
        {ok, A} -> A;
        {error, Reason} ->
            error({parse_error, Reason})
    end;
make({Type, Params}) when is_binary(Type) ->
    IOV = [Type, <<" ">> |
           ersip_iolist:join(<<", ">>,
                             [[K, <<"=">>, V] || {K, V} <- quote(Params)])],
    make(iolist_to_binary(IOV)).

%% @doc Parse header from binary or from ersip_hdr header.
%% @returns {ok, authinfo()} or {error, {invalid_authinfo, term()}}.
%% Example:
%% ```
%%   {ok, AuthInfo} = ersip_authinfo:parse(<<"Digest realm=\"testrealm@host.com\", qop=\"auth,auth-int\", nonce=dcd98b7102dd2f0e8b11d0f600bfb0c093">>),
%%   {error, _} = ersip_authinfo:parse(<<"@">>).
%% '''
-spec parse(binary()) -> parse_result().
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

%% @doc Assemble authorization info into iolist.
-spec assemble(authinfo()) -> iolist().
assemble(#authinfo{type = T, params = P}) ->
    [T, <<" ">>,
     ersip_iolist:join(<<", ">>,
                       [[K, <<"=">>, V] || {K, V} <- P])].

%% @doc Assemble authorization info into binary.
-spec assemble_bin(authinfo()) -> binary().
assemble_bin(#authinfo{} = A) ->
    iolist_to_binary(assemble(A)).

%% @doc Get raw value (in plain erlang types) of the header.
%% Plain format here is ``{Type :: binary(), [{LowerParameterName :: binary(), UnquotedValue : binary()}]}''
%% Example:
%% ```
%%   AuthInfo = ersip_authinfo:make(<<"Digest realm=\"testrealm@host.com\", QOP=\"auth,auth-int\", nonce=dcd98b7102dd2f0e8b11d0f600bfb0c093">>),
%%   {<<"digest">>,
%%    [{<<"realm">>, <<"testrealm@host.com">>},
%%     {<<"qop">>, <<"auth,auth-int">>},
%%     {<<"nonce">>, <<"dcd98b7102dd2f0e8b11d0f600bfb0c093">>}]}
%%     = ersip_authinfo:raw(AuthInfo).
%% '''
-spec raw(authinfo()) -> raw().
raw(#authinfo{params = P} = AI) ->
    {type(AI), unquote(P)}.

%%===================================================================
%% Internals
%%===================================================================

%% @private
-spec parse_params(binary()) -> ersip_parser_aux:parse_result(ersip_parser_aux:gen_param_list()).
parse_params(Binary) ->
    case ersip_parser_aux:parse_params($, , Binary) of
        {ok, Params, _} = Ok ->
            case validate_params(Params) of
                ok ->
                    Ok;
                {error, _} = Error ->
                    Error
            end;
        {error, _} = Error ->
            Error
    end.

%% @private
-spec validate_params(ersip_parser_aux:gen_param_list()) -> ok | {error, term()}.
validate_params([]) ->
    ok;
validate_params([{Key, <<>>} | _]) ->
    {error, {missing_value, Key}};
validate_params([{_, _} | Rest]) ->
    validate_params(Rest).

%% @private
%% @doc unquote all parameters in list
-spec unquote(ersip_parser_aux:gen_param_list()) -> [{binary(), binary()}].
unquote(List) ->
    [case V of
         <<"\"", _/binary>> ->
             {ok, Unquoted, <<>>} = ersip_parser_aux:unquoted_string(V),
             {ersip_bin:to_lower(K), Unquoted};
         _ -> {ersip_bin:to_lower(K), V}
     end || {K, V} <- List].

%% @private
%% @doc quote all parameters in list
-spec quote([{binary(), binary()}]) -> [{binary(), binary()}].
quote(List) ->
    [case ersip_parser_aux:check_token(V) of
         true -> {K, V};
         false -> {K, ersip_quoted_string:quote(V)}
     end || {K, V} <- List].
