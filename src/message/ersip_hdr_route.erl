%%%
%%% Copyright (c) 2018, 2020 Dmitry Poroh
%%% All rights reserved.
%%% Distributed under the terms of the MIT License. See the LICENSE file.
%%%
%%% SIP Route/Record-route header (single header).
%%%

-module(ersip_hdr_route).

-export([new/1,
         uri/1,
         set_uri/2,
         display_name/1,
         set_display_name/2,
         is_loose_route/1,
         params/1,
         param/2,
         set_param/3,
         make/1,
         assemble/1,
         assemble_bin/1,
         parse_hdr/1,
         parse/1,
         raw/1
        ]).

-export_type([route/0, raw/0, make/0]).

%%===================================================================
%% Types
%%===================================================================

-record(route, {display_name :: ersip_nameaddr:display_name(),
                uri          :: ersip_uri:uri(),
                hparams   = ersip_hparams:new() :: ersip_hparams:hparams()
               }).
-type route()        :: #route{}.
-type route_param()  :: {Key :: binary(), Value :: binary()}.
-type parse_result() :: {ok, route()} | {error, parse_error()}.
-type parse_error()  :: {invalid_route, term()}.
-type raw() :: #{uri          := ersip_uri:raw(),
                 params       := ersip_hparams:raw(),
                 display_name := ersip_display_name:raw()}.
-type make() :: binary()
              | #{uri          := ersip_uri:make(),
                  params       => ersip_hparams:make(),
                  display_name => ersip_display_name:make()}.

%%===================================================================
%% API
%%===================================================================

%% @doc Create Route header from SIP URI.
-spec new(ersip_uri:uri()) -> route().
new(URI) ->
    #route{display_name = {display_name, []}, uri = URI}.

%% @doc URI from Route header.
-spec uri(route()) -> ersip_uri:uri().
uri(#route{uri = URI}) ->
    URI.

%% @doc Set URI of Route header.
-spec set_uri(ersip_uri:uri(), route()) -> route().
set_uri(URI, #route{} = R) ->
    R#route{uri = URI}.

%% @doc Display name in Route header.
-spec display_name(route()) -> ersip_display_name:display_name().
display_name(#route{display_name = DN}) ->
    DN.

%% @doc Set display name of Route header.
-spec set_display_name(ersip_display_name:display_name(), route()) -> route().
set_display_name(DN, #route{} = Route) ->
    Route#route{display_name = DN}.

%% @doc Check if route contains loose router's URI.
-spec is_loose_route(route()) -> boolean().
is_loose_route(#route{uri = URI}) ->
    ersip_uri:loose_router(URI).

%% @doc Get parameters of Route header.
-spec params(route()) -> [route_param()].
params(#route{hparams = HP}) ->
    ersip_hparams:to_raw_list(HP).

%% @doc Get parameter of Route header.
-spec param(binary(), route()) -> {ok, binary()} | not_found.
param(Key, #route{hparams = HParams}) ->
    ersip_hparams:find_raw(Key, HParams).

%% @doc Set parameters of Route header.
-spec set_param(Key :: binary(), Value :: binary(), route()) -> route().
set_param(Key, Value, #route{hparams = HParams} = Route)
        when is_binary(Key), is_binary(Value) ->
    Route#route{hparams = ersip_hparams:set_raw(Key, Value, HParams)}.

%% @doc Make Route header from binary or from raw representation.
-spec make(make()) -> route().
make(Bin) when is_binary(Bin) ->
    case parse(Bin) of
        {ok, Route} -> Route;
        {error, Reason} -> error(Reason)
    end;
make(#{uri := URI} = Raw) ->
    HParams0 = ersip_hparams:make(maps:get(params, Raw, #{})),
    HParams =
        case ersip_hparams:parse_known(fun parse_known/2, HParams0) of
            {ok, H} -> H
            %% {error, Reason} -> error({invalid_params, Reason}) %% (Ref1) see below
        end,
    #route{display_name  = ersip_display_name:make(maps:get(display_name, Raw, <<>>)),
           uri           = ersip_uri:make(URI),
           hparams       = HParams
          }.

%% @doc Parse Route header from binary.
-spec parse(binary()) -> parse_result().
parse(Bin) ->
    case parse_hdr(Bin) of
        {ok, Route, <<>>} -> {ok, Route};
        {ok, _, _} -> {error, {invalid_route, Bin}};
        {error, Reason} -> {error, {invalid_route, Reason}}
    end.

%% @doc Parse single Route header and return unparsed rest.
-spec parse_hdr(binary()) -> ersip_parser_aux:parse_result(route()).
parse_hdr(Bin) ->
    Parsers = [fun ersip_nameaddr:parse/1,
               fun ersip_parser_aux:trim_lws/1,
               fun parse_params/1,
               fun ersip_parser_aux:trim_lws/1
              ],
    case ersip_parser_aux:parse_all(Bin, Parsers) of
        {ok, [{DisplayName, URI}, _, HParams, _], Rest} ->
            Route = #route{display_name = DisplayName,
                           uri = URI,
                           hparams = HParams},
            {ok, Route, Rest};
        {error, Reason} ->
            {error, {invalid_route, Reason}}
    end.

%% @doc Assemble route header to iolist().
-spec assemble(route()) -> iolist().
assemble(#route{display_name = DN, uri = URI, hparams = HParams}) ->
    HParamsIO0 = ersip_hparams:assemble(HParams),
    HParamsIO =
        case ersip_iolist:is_empty(HParamsIO0) of
            true -> [];
            false -> [$; | HParamsIO0]
        end,
    [ersip_nameaddr:assemble(DN, URI), HParamsIO].

%% @doc Assemble route header to binary().
-spec assemble_bin(route()) -> binary().
assemble_bin(#route{} = R) ->
    iolist_to_binary(assemble(R)).

%% @doc Raw representation of Route header.
-spec raw(route()) -> raw().
raw(#route{} = Route) ->
    #{uri => ersip_uri:raw(uri(Route)),
      display_name => ersip_display_name:raw(display_name(Route)),
      params => ersip_hparams:raw(Route#route.hparams)
     }.

%%===================================================================
%% Helpers
%%===================================================================

%% @private
-spec parse_params(binary()) -> ersip_parser_aux:parse_result(ersip_parser_aux:gen_param_list()).
parse_params(<<$;, Bin/binary>>) ->
    ersip_hparams:parse(fun parse_known/2, Bin);
parse_params(Bin) ->
    {ok, ersip_hparams:new(), Bin}.

%% @private
%%
%% WARNING: If you add known parameter here then you need to add handling
%% parse known result above (see Ref1).
-spec parse_known(binary(), binary()) -> ersip_hparams:parse_known_fun_result().
parse_known(_, _) ->
    {ok, unknown}.
