%%
%% Copyright (c) 2018 Dmitry Poroh
%% All rights reserved.
%% Distributed under the terms of the MIT License. See the LICENSE file.
%%
%% SIP From/To headers
%%

-module(ersip_hdr_fromto).

-export([new/0,
         make/1,
         display_name/1,
         uri/1,
         tag/1,
         set_tag/2,
         tag_key/1,
         params/1,
         parse/1,
         build/2,
         assemble/1
        ]).
-export_type([fromto/0,
              tag/0,
              tag_key/0
             ]).

%%%===================================================================
%%% Types
%%%===================================================================

-record(fromto, {
          display_name = undefined :: ersip_nameaddr:display_name(),
          uri                      :: ersip_uri:uri(),
          params = #{}             :: fromto_params()
         }).

-type fromto_params() :: #{tag => tag(),
                           binary() => binary()
                          }.
-type known_fromto_params() :: tag.
-type fromto()  :: #fromto{}.
-type tag()     :: {tag, binary()}.
-type tag_key() :: {tag_key, binary()}.

%%%===================================================================
%%% API
%%%===================================================================

-spec new() -> fromto().
new() ->
    DefaultHost = ersip_host:make({127, 0, 0, 1}),
    #fromto{uri = ersip_uri:make([{host, DefaultHost}])}.

-spec make(binary()) -> fromto().
make(Bin) ->
    case parse_fromto(Bin) of
        {ok, FromTo} ->
            FromTo;
        {error, _} = Error ->
            error(Error)
    end.

-spec display_name(fromto()) -> ersip_nameaddr:display_name().
display_name(#fromto{display_name = DN}) ->
    DN.

-spec uri(fromto()) -> ersip_uri:uri().
uri(#fromto{uri = URI}) ->
    URI.

-spec params(fromto()) -> fromto_params().
params(#fromto{params = P}) ->
    P.

-spec tag(fromto()) -> undefined | tag().
tag(#fromto{} = FT) ->
    P = params(FT),
    case maps:find(tag, P) of
        {ok, Tag} ->
            Tag;
        error ->
            undefined
    end.

-spec set_tag(tag() | undefined, fromto()) -> fromto().
set_tag({tag, _} = Tag, #fromto{} = FT) ->
    OldParams = params(FT),
    NewParams = OldParams#{tag => Tag},
    set_params(NewParams, FT);
set_tag(undefined, #fromto{} = FT) ->
    OldParams = params(FT),
    NewParams = maps:remove(tag, OldParams),
    set_params(NewParams, FT).

-spec tag_key(tag() | fromto()) -> undefined | tag_key().
tag_key({tag, T}) ->
    {tag_key, ersip_bin:to_lower(T)};
tag_key(#fromto{} = FT) ->
    case tag(FT) of
        undefined ->
            undefined;
        {tag, _} = Tag ->
            tag_key(Tag)
    end.

-spec parse(ersip_hdr:header()) -> {ok, fromto()}
                                       | {error, Error} when
      Error :: {einval, address}
             | no_value
             | multiple_values.
parse(Header) ->
    case ersip_hdr:raw_values(Header) of
        [] ->
            {error, no_value};
        [V] ->
            parse_fromto(iolist_to_binary(V));
        [_| _] ->
            {error, multiple_values}
    end.

-spec build(HdrName, fromto()) -> ersip_hdr:header() when
      HdrName :: binary().
build(HdrName, #fromto{} = FromTo) ->
    Hdr = ersip_hdr:new(HdrName),
    ersip_hdr:add_value(assemble(FromTo), Hdr).

-spec assemble(fromto()) -> iolist().
assemble(#fromto{} = FromTo) ->
    DisplayName = display_name(FromTo),
    URI = uri(FromTo),
    NameAddr = ersip_nameaddr:assemble(DisplayName, URI),
    [NameAddr, assemble_params(params(FromTo))];
assemble({tag, TagBin}) when is_binary(TagBin) ->
    TagBin.

%%%===================================================================
%%% Internal Implementation
%%%===================================================================

-spec set_params(fromto_params(), fromto()) -> fromto().
set_params(Params, #fromto{} = FT) ->
    FT#fromto{params = Params}.

-spec parse_fromto(binary()) -> {ok, fromto()}
                                    | {error, Error} when
      Error :: {einval, address}.
parse_fromto(Bin) ->
    Parsers = [fun ersip_nameaddr:parse/1,
               fun ersip_parser_aux:trim_lws/1,
               fun parse_params/1
              ],
    case ersip_parser_aux:parse_all(Bin, Parsers) of
        {ok, [{DisplayName, URI}, _, ParamsList], <<>>} ->
            {ok,
             #fromto{display_name = DisplayName,
                     uri          = URI,
                     params       = maps:from_list(ParamsList)
                    }
            };
        {error, _} = Error ->
            Error
    end.

parse_params(<<$;, Bin/binary>>) ->
    parse_params(Bin);
parse_params(<<>>) ->
    {ok, [], <<>>};
parse_params(Bin) ->
    ersip_parser_aux:parse_kvps(fun fromto_params_validator/2,
                                <<";">>,
                                Bin).

fromto_params_validator(<<"tag">>, Value) ->
    %% tag-param   =  "tag" EQUAL token
    case ersip_parser_aux:check_token(Value) of
        true ->
            {ok, {tag, {tag, Value}}};
        false ->
            {error, {invalid_tag, Value}}
    end;
fromto_params_validator(Key, novalue) ->
    case ersip_parser_aux:check_token(Key) of
        true ->
            {ok, {ersip_bin:to_lower(Key), novalue}};
        false ->
            {error, {invalid_gen_param, Key}}
    end;
fromto_params_validator(Key, Value) ->
    case ersip_parser_aux:parse_gen_param_value(Value) of
        {ok, ParsedValue, <<>>} ->
            {ok, {ersip_bin:to_lower(Key), ParsedValue}};
        _ ->
            {error, {invalid_gen_param, {Key, Value}}}
    end.

-spec assemble_params(fromto_params()) -> [iolist()].
assemble_params(Params) ->
    lists:map(fun assemble_param/1,
              maps:to_list(Params)).

-spec assemble_param({Name, Value}) -> iolist() when
      Name :: known_fromto_params()
            | binary(),
      Value :: term().
assemble_param({tag, {tag, Value}}) ->
    [<<";tag=">>, Value];
assemble_param({Name, novalue}) ->
    <<";", Name/binary>>;
assemble_param({Name, Value}) ->
    <<";", Name/binary, "=", Value/binary>>.
