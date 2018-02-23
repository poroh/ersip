%%
%% Copyright (c) 2018 Dmitry Poroh
%% All rights reserved.
%% Distributed under the terms of the MIT License. See the LICENSE file.
%%
%% SIP From/To headers
%%

-module(ersip_hdr_fromto).

-export([ make/1,
          display_name/1,
          uri/1,
          tag/1,
          tag_key/1,
          params/1,
          parse/1 ]).
-export_type([ fromto/0,
               tag/0,
               tag_key/0
             ]).

%%%===================================================================
%%% Types
%%%===================================================================

-record(fromto, {
          display_name = undefined :: undefined | binary(),
          uri                      :: ersip_uri:uri(),
          params = #{}             :: fromto_params()
         }).

-type fromto_params() :: #{ tag => tag(),
                            binary() => binary()
                          }.
-type fromto()  :: #fromto{}.
-type tag()     :: { tag, binary() }.
-type tag_key() :: { tag_key, binary() }.

%%%===================================================================
%%% API
%%%===================================================================

-spec make(binary()) -> fromto().
make(Bin) ->
    case parse_fromto(Bin) of
        { ok, FromTo } ->
            FromTo;
        { error, _ } = Error ->
            error(Error)
    end.

-spec display_name(fromto()) -> undefined | binary().
display_name(#fromto{ display_name = DN }) ->
    DN.

-spec uri(fromto()) -> ersip_uri:uri().
uri(#fromto{ uri = URI }) ->
    URI.

-spec params(fromto()) -> fromto_params().
params(#fromto{ params = P }) ->
    P.

-spec tag(fromto()) -> undefined | tag().
tag(#fromto{} = FT) ->
    P = params(FT),
    case maps:find(tag, P) of
        { ok, Tag } ->
            Tag;
        error ->
            undefined
    end.

-spec tag_key(fromto()) -> undefined | tag_key().
tag_key(FT) ->
    case tag(FT) of
        undefined ->
            undefined;
        { tag, T } ->
            { tag_key, ersip_bin:to_lower(T) }
    end.

-spec parse(ersip_hdr:header()) -> { ok, fromto() }
                                 | { error, Error } when
      Error :: { einval, address }
             | no_value
             | multiple_values.
parse(Header) ->
    case ersip_hdr:raw_values(Header) of
        [] ->
            { error, no_value };
        [ V ] ->
            parse_fromto(iolist_to_binary(V));
        [ _| _ ] ->
            { error, multiple_values }
    end.

-spec parse_fromto(binary()) -> { ok, fromto() }
                              | { error, Error } when
      Error :: { einval, address }.
parse_fromto(Bin) ->
    Parsers = [ fun ersip_nameaddr:parse/1,
                fun parse_params/1
              ],
    case ersip_parser_aux:parse_all(Bin, Parsers) of
        { ok, [ { DisplayName, URI }, ParamsList ], <<>> } ->
            { ok,
              #fromto{ display_name = DisplayName,
                       uri          = URI,
                       params       = maps:from_list(ParamsList)
                     }
            };
        { error, _ } = Error ->
            Error
    end.

parse_params(<<$;, Bin/binary>>) ->
    parse_params(Bin);
parse_params(<<>>) ->
    { ok, [], <<>> };
parse_params(Bin) ->
    ersip_parser_aux:parse_kvps(fun fromto_params_validator/2,
                                <<";">>,
                                Bin).

fromto_params_validator(<<"tag">>, Value) ->
    %% tag-param   =  "tag" EQUAL token
    case ersip_parser_aux:check_token(Value) of
        true ->
            { ok, { tag, { tag, Value } } };
        false ->
            { error, { invalid_tag, Value } }
    end;
fromto_params_validator(Key, Value) ->
    case ersip_parser_aux:parse_gen_param_value(Value) of
        { ok, ParsedValue, <<>> } ->
            { ok, { ersip_bin:to_lower(Key), ParsedValue } };
        _ ->
            { error, { invalid_gen_param, { Key, Value } } }
    end.
