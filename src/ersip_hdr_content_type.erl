%%
%% Copyright (c) 2018 Dmitry Poroh
%% All rights reserved.
%% Distributed under the terms of the MIT License. See the LICENSE file.
%%
%% SIP Content-type header
%%

-module(ersip_hdr_content_type).

-export([ mime_type/1,
          params/1,
          make/1,
          parse/1,
          build/2,
          assemble/1
        ]).

-export_type([ content_type/0,
               mime_type/0 
             ]).

%%%===================================================================
%%% Types
%%%===================================================================

-record(content_type, {
          type    :: mime_type(),
          params  :: params()
         }).
-type mime_type() :: { mime, Type :: binary(), SubType :: binary() }.
-type content_type() :: #content_type{}.
-type params() :: [ pair() ].
-type pair() :: { Key :: binary(), Value :: binary() }.

%%%===================================================================
%%% API
%%%===================================================================

-spec mime_type(content_type()) -> mime_type().
mime_type(#content_type{ type = T }) -> 
    T.

-spec params(content_type()) -> params().
params(#content_type{ params = P }) -> 
    P.

-spec make(ersip_hdr:header() | binary()) -> content_type().
make(Bin) when is_binary(Bin) ->
    case parse_content_type(Bin) of
        { ok, Content_Type } ->
            Content_Type;
        Error ->
            error(Error)
    end;
make(Header) ->
    case parse(Header) of
        { ok, Content_Type } ->
            Content_Type;
        Error ->
            error(Error)
    end.

-spec parse(ersip_hdr:header()) -> Result when
      Result :: { ok, content_type() }
              | { error, Error },
      Error :: no_content_type
             | { invalid_content_type, binary() }.
parse(Header) ->
    case ersip_hdr:raw_values(Header) of
        [] ->
            { error, no_content_type };
        [ Content_TypeIOList ]  ->
            parse_content_type(iolist_to_binary(Content_TypeIOList));
        _ ->
            { error, multiple_content_types }
    end.

-spec build(HeaderName :: binary(), content_type()) -> ersip_hdr:header().
build(HdrName, #content_type{} = ContentType) ->
    Hdr = ersip_hdr:new(HdrName),
    ersip_hdr:add_value([ assemble(ContentType) ], Hdr).

-spec assemble(content_type()) ->  binary().
assemble(#content_type{} = ContentType) ->
    { mime, Type, SubType } = mime_type(ContentType),
    [ Type, <<"/">>, SubType,
      lists:map(fun({ Key, Value }) ->
                        [ $;, Key, $=, Value ]
                end,
                params(ContentType))
    ].

%%%===================================================================
%%% Internal implementation
%%%===================================================================

%% media-type     = type "/" subtype *( ";" parameter )
%% type           = token
%% subtype        = token
-spec parse_content_type(binary()) -> Result when
      Result :: { ok, content_type() }
              | { error, Error },
      Error  :: { invalid_content_type, binary() }.
parse_content_type(Binary) ->
    Parsers = [ fun ersip_parser_aux:parse_token/1,
                ersip_parser_aux:make_sep_parser($/),
                fun ersip_parser_aux:parse_token/1,
                fun ersip_parser_aux:trim_lws/1,
                fun parse_params/1
              ],
    case ersip_parser_aux:parse_all(Binary, Parsers) of
        { ok, [  Type, _, SubType, _, ParamsList ], <<>> } ->
            { ok,
              #content_type{ type    = { mime,
                                         ersip_bin:to_lower(Type),
                                         ersip_bin:to_lower(SubType) },
                             params  = ParamsList
                           }
            };
        _ ->
            { error, { invalid_content_type, Binary } }
    end.

parse_params(<<$;, Bin/binary>>) ->
    parse_params(Bin);
parse_params(<<>>) ->
    { ok, [], <<>> };
parse_params(Bin) ->
    ersip_parser_aux:parse_kvps(fun params_validator/2,
                                <<";">>,
                                Bin).

%% parameter               = attribute "=" value
%% attribute               = token
%% value                   = token | quoted-string    
params_validator(Key, Value) ->
    case parse_parameter(Value) of
        { ok, ParsedValue, <<>> } ->
            { ok, { ersip_bin:to_lower(Key), ParsedValue } };
        _ ->
            { error, { invalid_parameter, { Key, Value } } }
    end.

parse_parameter(Bin) ->
    case ersip_parser_aux:quoted_string(Bin) of
        { ok, Val, <<>> } ->
            { ok, Val, <<>> };
        error ->
            case ersip_parser_aux:parse_token(Bin) of
                { ok, _, <<>> } = R ->
                    R;
                _ ->
                    { error, { invalid_parameter, Bin } }
            end
    end.
