%%%
%%% Copyright (c) 2018, 2020 Dmitry Poroh
%%% All rights reserved.
%%% Distributed under the terms of the MIT License. See the LICENSE file.
%%%
%%% SIP From/To headers
%%%

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
         assemble_bin/1,
         raw/1
        ]).
-export_type([fromto/0,
              tag/0,
              tag_key/0,
              raw/0
             ]).

%%===================================================================
%% Types
%%===================================================================

-record(fromto, {display_name = undefined      :: ersip_nameaddr:display_name(),
                 uri                           :: ersip_uri:uri(),
                 hparams = ersip_hparams:new() :: ersip_hparams:hparams()
                }).

-type fromto()   :: #fromto{}.
-type tag()      :: {tag, binary()}.
-type tag_key()  :: {tag_key, binary()}.

-type uri()      :: ersip_uri:raw().
-type uri_bin()  :: ersip_uri:raw() | binary().

-type raw(T)     :: #{uri          := T,
                      params       := ersip_hparams:raw(),
                      display_name := ersip_display_name:raw(),
                      tag          => binary(),
                      tag_key      => binary()}.


-type raw()      :: raw(uri()).
-type raw_make() :: raw(uri_bin()).


-type parse_result() :: {ok, fromto()} | {error, parse_error()}.
-type parse_error()  :: {invalid_fromto, no_value}
                      | {invalid_fromto, multiple_values}
                      | {invalid_fromto, term()}.
%% deprecated:
-type fromto_params() :: #{tag => tag(), binary() => binary()}.


%%===================================================================
%% API
%%===================================================================

%% @doc Create empty From/To field. (sip:127.0.0.1).
-spec new() -> fromto().
new() ->
    DefaultHost = ersip_host:make({127, 0, 0, 1}),
    #fromto{display_name = {display_name, []},
            uri = ersip_uri:make([{host, DefaultHost}])}.

%% @doc Make From/To field from binary() or raw representation.
-spec make(binary() | raw_make()) -> fromto().
make(Bin) when is_binary(Bin) ->
    case parse_fromto(Bin) of
        {ok, FromTo} ->
            FromTo;
        {error, _} = Error ->
            error(Error)
    end;
make(#{uri := URI} = Raw) ->
    HParams0 = ersip_hparams:make(maps:get(params, Raw, #{})),
    HParams =
        case ersip_hparams:parse_known(fun parse_known/2, HParams0) of
            {ok, H} -> H;
            {error, Reason} -> error({invalid_params, Reason})
        end,
    FT0 = #fromto{display_name  = ersip_display_name:make(maps:get(display_name, Raw, <<>>)),
                  uri           = ersip_uri:make(URI),
                  hparams       = HParams
                 },

    Opts = [{tag, fun(X, FT) -> set_tag({tag, X}, FT) end}],
    ersip_map:apply_to(Opts, Raw, FT0).

%% @doc Display name of From/To fields.
-spec display_name(fromto()) -> ersip_nameaddr:display_name().
display_name(#fromto{display_name = DN}) ->
    DN.

%% @doc Set display name of From/To fields.
-spec set_display_name(ersip_nameaddr:display_name(), fromto()) -> fromto().
set_display_name(DN, #fromto{} = FromTo) ->
    FromTo#fromto{display_name = DN}.

%% @doc URI of From/To fields.
-spec uri(fromto()) -> ersip_uri:uri().
uri(#fromto{uri = URI}) ->
    URI.

%% @doc Set URI of From/To fields.
-spec set_uri(ersip_uri:uri(), fromto()) -> fromto().
set_uri(URI, #fromto{} = FromTo) ->
    FromTo#fromto{uri = URI}.

%% @deprecated
-spec params(fromto()) -> fromto_params().
params(#fromto{hparams = HP}) ->
    maps:from_list(ersip_hparams:to_list(HP)).

%% @doc Raw params of the From/To header.
-spec raw_params(fromto()) -> [{binary(), binary()} | binary()].
raw_params(#fromto{hparams = HP}) ->
    ersip_hparams:to_raw_list(HP).

%% @doc Tag parameter of From/To header.
-spec tag(fromto()) -> undefined | tag().
tag(#fromto{hparams = HParams}) ->
    case ersip_hparams:find(tag, HParams) of
        not_found -> undefined;
        {ok, V} -> V
    end.

%% @doc Set tag parameter of From/To header.
%% Note that set_tag(undefined, ...) is deprecated, please use clear_tag().
-spec set_tag(tag() | undefined, fromto()) -> fromto().
set_tag({tag, TagV} = Tag, #fromto{hparams = HParams} = FT) ->
    NewHParams = ersip_hparams:set(tag, Tag, <<"tag">>, TagV, HParams),
    FT#fromto{hparams = NewHParams};
set_tag(undefined, #fromto{} = FT) ->
    clear_tag(FT).

%% @doc Clear tag parameter of From/To header.
-spec clear_tag(fromto()) -> fromto().
clear_tag(#fromto{hparams = HParams} = FT) ->
    NewHParams = ersip_hparams:remove(tag, HParams),
    FT#fromto{hparams = NewHParams}.

%% @doc Set random tag parameter of From/To header.
%% Number entropy bytes are specified by first parameter.
-spec set_random_tag(pos_integer(), fromto()) -> fromto().
set_random_tag(NumBytes, #fromto{} = FT) ->
    Tag = {tag, ersip_id:alphanum(crypto:strong_rand_bytes(NumBytes))},
    set_tag(Tag, FT).

%% @doc Erlang-comparable tag. It can be used for dialog-matching for
%% example in ETS.
-spec tag_key(tag() | fromto()) -> undefined | tag_key().
tag_key({tag, T}) ->
    {tag_key, ersip_bin:to_lower(T)};
tag_key(#fromto{} = FT) ->
    case tag(FT) of
        undefined -> undefined;
        {tag, _} = Tag -> tag_key(Tag)
    end.

%% @doc Parse header from SIP raw header or from binary().
-spec parse(ersip_hdr:header() | binary()) -> parse_result().
parse(Header) when is_binary(Header) ->
    parse_fromto(Header);
parse(Header) ->
    case ersip_hdr:raw_values(Header) of
        [] ->  {error, no_value};
        [V] -> parse_fromto(iolist_to_binary(V));
        [_| _] -> {error, {invalid_fromto, multiple_values}}
    end.

%% @doc Build SIP raw header.
-spec build(binary(), fromto()) -> ersip_hdr:header().
build(HdrName, #fromto{} = FromTo) ->
    Hdr = ersip_hdr:new(HdrName),
    ersip_hdr:add_value(assemble(FromTo), Hdr).

%% @doc Serialize header or tag to iolist().
-spec assemble(fromto()|tag()) -> iolist().
assemble(#fromto{hparams = HParams} = FromTo) ->
    DisplayName = display_name(FromTo),
    URI = uri(FromTo),
    NameAddr = ersip_nameaddr:assemble(DisplayName, URI),
    HParamsIO0 = ersip_hparams:assemble(HParams),
    HParamsIO =
        case ersip_iolist:is_empty(HParamsIO0) of
            true -> [];
            false -> [$; | HParamsIO0]
        end,
    [NameAddr, HParamsIO];
assemble({tag, TagBin}) when is_binary(TagBin) ->
    [TagBin].

%% @doc Serialize header to binary().
-spec assemble_bin(fromto()) -> binary().
assemble_bin(#fromto{} = FromTo) ->
    iolist_to_binary(assemble(FromTo)).

%% @doc Raw representation of SIP header.
-spec raw(fromto()) -> raw().
raw(#fromto{} = FT) ->
    #fromto{display_name = DN, uri = URI, hparams = HParams} = FT,
    Raw = #{uri    => ersip_uri:raw(URI),
            params => ersip_hparams:raw(HParams),
            display_name => ersip_display_name:raw(DN)
           },
    Opts = [{tag, tag(FT), fun({tag, X}) -> X end}],
    OptsKVP = [{K, F(V)} || {K, V, F} <- Opts, V /= undefined],
    maps:merge(maps:from_list(OptsKVP), Raw).

%%===================================================================
%% Internal Implementation
%%===================================================================

%% @private
-spec parse_fromto(binary()) -> parse_result().
parse_fromto(Bin) ->
    Parsers = [fun(NameAddr) -> ersip_nameaddr:parse(NameAddr, [<<" ">>, <<";">>, <<"\t">>]) end,
               fun ersip_parser_aux:trim_lws/1,
               fun parse_params/1
              ],
    case ersip_parser_aux:parse_all(Bin, Parsers) of
        {ok, [{DisplayName, URI}, _, HParams], <<>>} ->
            FromTo = #fromto{display_name = DisplayName,
                             uri          = URI,
                             hparams      = HParams},
            {ok, FromTo};
        {ok, _, _} -> {error, {invalid_fromto, Bin}};
        {error, Reason} -> {error, {invalid_fromto, Reason}}
    end.

%% @private
-spec parse_params(binary()) -> ersip_parser_aux:parse_result(ersip_parser_aux:gen_param_list()).
parse_params(<<$;, Bin/binary>>) ->
    ersip_hparams:parse(fun parse_known/2, Bin);
parse_params(Bin) ->
    {ok, ersip_hparams:new(), Bin}.

%% @private
-spec parse_known(binary(), binary()) -> ersip_hparams:parse_known_fun_result().
parse_known(<<"tag">>, Value) ->
    case ersip_parser_aux:check_token(Value) of
        true ->  {ok, {tag, {tag, Value}}};
        false -> {error, {invalid_tag, Value}}
    end;
parse_known(_, _) ->
    {ok, unknown}.
