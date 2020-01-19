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
         set_display_name/2,
         uri/1,
         set_uri/2,
         tag/1,
         set_tag/2,
         set_random_tag/2,
         tag_key/1,
         params/1,
         raw_params/1,
         parse/1,
         build/2,
         assemble/1,
         assemble_bin/1
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
    #fromto{display_name = {display_name, []},
            uri = ersip_uri:make([{host, DefaultHost}])}.

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

-spec set_display_name(ersip_nameaddr:display_name(), fromto()) -> fromto().
set_display_name(DN, #fromto{} = FromTo) ->
    FromTo#fromto{display_name = DN}.

-spec uri(fromto()) -> ersip_uri:uri().
uri(#fromto{uri = URI}) ->
    URI.

-spec set_uri(ersip_uri:uri(), fromto()) -> fromto().
set_uri(URI, #fromto{} = FromTo) ->
    FromTo#fromto{uri = URI}.

-spec params(fromto()) -> fromto_params().
params(#fromto{params = P}) ->
    P.

-spec raw_params(fromto()) -> [{binary(), binary()} | binary()].
raw_params(#fromto{params = Params}) ->
    lists:map(fun raw_param/1, maps:to_list(Params)).

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

-spec set_random_tag(pos_integer(), fromto()) -> fromto().
set_random_tag(NumBytes, #fromto{} = FT) ->
    Tag = {tag, ersip_id:token(crypto:strong_rand_bytes(NumBytes))},
    set_tag(Tag, FT).

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

-spec parse(ersip_hdr:header() | binary()) -> {ok, fromto()}
                                            | {error, Error} when
      Error :: {einval, address}
             | no_value
             | multiple_values.
parse(Header) when is_binary(Header) ->
    parse_fromto(Header);
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

-spec assemble_bin(fromto()) -> binary().
assemble_bin(#fromto{} = FromTo) ->
    iolist_to_binary(assemble(FromTo)).

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
    Parsers = [fun(NameAddr) -> ersip_nameaddr:parse(NameAddr, [<<" ">>, <<";">>, <<"\t">>]) end,
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

-spec parse_params(binary()) -> ersip_parser_aux:parse_result(ersip_parser_aux:gen_param_list()).
parse_params(<<$;, Bin/binary>>) ->
    parse_params(Bin);
parse_params(<<>>) ->
    {ok, [], <<>>};
parse_params(Bin) ->
    case ersip_parser_aux:parse_params($;, Bin) of
        {ok, Params, <<>>} ->
            do_parse_params(Params, []);
        _ ->
            {error, {invalid_parameters, Bin}}
    end.

-spec do_parse_params(ersip_parser_aux:gen_param_list(), ersip_parser_aux:gen_param_list()) ->
                             ersip_parser_aux:parse_result(ersip_parser_aux:gen_param_list()).
do_parse_params([], Acc) ->
    {ok, lists:reverse(Acc), <<>>};
do_parse_params([{<<"tag">>, Value} | Rest], Acc) ->
    %% tag-param   =  "tag" EQUAL token
    case ersip_parser_aux:check_token(Value) of
        true ->
            do_parse_params(Rest, [{tag, {tag, Value}} | Acc]);
        false ->
            {error, {invalid_tag, Value}}
    end;
do_parse_params([{Key, <<>>} | Rest], Acc) ->
    Acc1 = [{ersip_bin:to_lower(Key), novalue} | Acc],
    do_parse_params(Rest, Acc1);
do_parse_params([{Key, Value} | Rest], Acc) ->
    Acc1 = [{ersip_bin:to_lower(Key), Value} | Acc],
    do_parse_params(Rest, Acc1).

-spec assemble_params(fromto_params()) -> [iolist()].
assemble_params(Params) ->
    lists:map(fun assemble_param/1,
              maps:to_list(Params)).


-spec raw_param({known_fromto_params() | binary(), term()}) -> {binary(), binary()} | binary().
raw_param({tag, {tag, Value}}) -> {<<"tag">>, Value};
raw_param({Name, novalue}) -> Name;
raw_param({Name, Value})   -> {Name, Value}.

-spec assemble_param({known_fromto_params() | binary(), term()}) -> iolist().
assemble_param({_, _} = Pair) ->
    case raw_param(Pair) of
        {Name, Val} -> [<<";">>, Name, <<"=">>, Val];
        Name -> [<<";">>, Name]
    end.
