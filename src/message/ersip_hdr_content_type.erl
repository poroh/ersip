%%%
%%% Copyright (c) 2018, 2020 Dmitry Poroh
%%% All rights reserved.
%%% Distributed under the terms of the MIT License. See the LICENSE file.
%%%
%%% SIP Content-type header
%%%

-module(ersip_hdr_content_type).

-export([mime_type/1,
         params/1,
         make/1,
         parse/1,
         build/2,
         assemble/1,
         assemble_bin/1,
         raw/1
        ]).

-export_type([content_type/0,
              mime_type/0,
              raw/0
             ]).

%%===================================================================
%% Types
%%===================================================================

-record(content_type, {type    :: mime_type(),
                       params  :: params()
                      }).
-type mime_type() :: {mime, Type :: binary(), SubType :: binary()}.
-type content_type() :: #content_type{}.
-type params() :: [pair()].
-type pair() :: {param_key(), param_value()}.
-type parse_result() :: {ok, content_type()} | {error, parse_error()}.
-type parse_error()  :: no_content_type
                      | {invalid_content_type, binary()}.
-type raw() :: #{type   := {binary(), binary()},
                 params := #{lower_param_key() => param_value()}}.
-type param_key()   :: binary().
-type param_value() :: binary().
-type lower_param_key() :: binary().

%%===================================================================
%% API
%%===================================================================

%% @doc MIME type pair without parameters.
-spec mime_type(content_type()) -> mime_type().
mime_type(#content_type{type = T}) ->
    T.

%% @doc Parameters of MIME type.
-spec params(content_type()) -> params().
params(#content_type{params = P}) ->
    P.

%% @doc Create Content-Type header from binary or from raw SIP header.
-spec make(ersip_hdr:header() | binary()) -> content_type().
make(Bin) when is_binary(Bin) ->
    case parse_content_type(Bin) of
        {ok, Content_Type} ->
            Content_Type;
        Error ->
            error(Error)
    end;
make(#{type := {Type, SubType}} = Raw) when is_map(Raw) ->
    ParamMap = maps:get(params, Raw, #{}),
    #content_type{type = {mime, Type, SubType},
                  params = quote(maps:to_list(ParamMap))};
make(Header) ->
    case parse(Header) of
        {ok, Content_Type} ->
            Content_Type;
        Error ->
            error(Error)
    end.

%% @doc Parse Content-Type raw SIP header.
-spec parse(ersip_hdr:header()) -> parse_result().
parse(Header) ->
    case ersip_hdr:raw_values(Header) of
        [] ->
            {error, no_content_type};
        [Content_TypeIOList]  ->
            parse_content_type(iolist_to_binary(Content_TypeIOList));
        _ ->
            {error, multiple_content_types}
    end.

%% @doc Build raw SIP header.
-spec build(HeaderName :: binary(), content_type()) -> ersip_hdr:header().
build(HdrName, #content_type{} = ContentType) ->
    Hdr = ersip_hdr:new(HdrName),
    ersip_hdr:add_value([assemble(ContentType)], Hdr).

%% @doc Serialize to iolist.
-spec assemble(content_type()) -> iolist().
assemble(#content_type{} = ContentType) ->
    {mime, Type, SubType} = mime_type(ContentType),
    [Type, <<"/">>, SubType,
     lists:map(fun({Key, Value}) ->
                       [$;, Key, $=, Value]
               end,
               params(ContentType))
    ].

%% @doc Serialize to binary.
-spec assemble_bin(content_type()) -> binary().
assemble_bin(#content_type{} = CT) ->
    iolist_to_binary(assemble(CT)).

%% @doc Raw value of the Content-Type.
-spec raw(content_type()) -> raw().
raw(#content_type{type = T, params = P}) ->
    {mime, Type, SubType} = T,
    ParamsL = [{ersip_bin:to_lower(K), ersip_quoted_string:unquote(V)} || {K, V} <- P],
    #{type => {Type, SubType},
      params => maps:from_list(ParamsL)}.

%%===================================================================
%% Internal implementation
%%===================================================================

%% @private
%% @doc
%%
%% ```
%% media-type     =  m-type SLASH m-subtype *(SEMI m-parameter)
%% m-type           =  discrete-type / composite-type
%% discrete-type    =  "text" / "image" / "audio" / "video"
%%                     / "application" / extension-token
%% composite-type   =  "message" / "multipart" / extension-token
%% extension-token  =  ietf-token / x-token
%% ietf-token       =  token
%% x-token          =  "x-" token
%% m-subtype        =  extension-token / iana-token
%% iana-token       =  token
%% m-parameter      =  m-attribute EQUAL m-value
%% m-attribute      =  token
%% m-value          =  token / quoted-string
%% '''
-spec parse_content_type(binary()) -> parse_result().
parse_content_type(Binary) ->
    Parsers = [fun ersip_parser_aux:parse_token/1,
               fun ersip_parser_aux:parse_slash/1,
               fun ersip_parser_aux:parse_token/1,
               fun ersip_parser_aux:trim_lws/1,
               fun parse_params/1
              ],
    case ersip_parser_aux:parse_all(Binary, Parsers) of
        {ok, [ Type, _, SubType, _, ParamsList], <<>>} ->
            {ok,
             #content_type{type = {mime,
                                   ersip_bin:to_lower(Type),
                                   ersip_bin:to_lower(SubType)},
                           params  = ParamsList}
            };
        _ ->
            {error, {invalid_content_type, Binary}}
    end.

parse_params(<<$;, Bin/binary>>) ->
    parse_params(Bin);
parse_params(<<>>) ->
    {ok, [], <<>>};
parse_params(Bin) ->
    ersip_parser_aux:parse_params($;, Bin).

%% @private
%% @doc quote all parameters in list
-spec quote([{binary(), binary()}]) -> [{binary(), binary()}].
quote(List) ->
    [case ersip_parser_aux:check_token(V) of
         true -> {K, V};
         false -> {K, ersip_quoted_string:quote(V)}
     end || {K, V} <- List].
