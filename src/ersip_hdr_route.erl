%%
%% Copyright (c) 2018 Dmitry Poroh
%% All rights reserved.
%% Distributed under the terms of the MIT License. See the LICENSE file.
%%
%% SIP Route/Record-route headers
%%

-module(ersip_hdr_route).

-export([uri/1,
         is_loose_route/1,
         params/1,
         set_param/3,
         make/1,
         parse/1,
         build/2,
         make_route/1
        ]).

-export_type([route/0]).

-include("ersip_uri.hrl").

%%%===================================================================
%%% Types
%%%===================================================================

-record(route, {display_name :: ersip_nameaddr:display_name(),
                uri          :: ersip_uri:uri(),
                params = []  :: [route_param()]
               }).
-type route()     :: #route{}.
-type route_set() :: ersip_route_set:route_set().
-type route_param() :: {Key :: binary(), Value :: binary()}.
-type parse_result() :: {ok, route_set()}
                      | {error, term()}.

-type maybe_rev_route_set() :: {ok, route_set()}
                             | {error, term()}.
%%%===================================================================
%%% API
%%%===================================================================

-spec uri(route()) -> ersip_uri:uri().
uri(#route{uri = URI}) ->
    URI.

-spec is_loose_route(route()) -> boolean().
is_loose_route(#route{uri = URI}) ->
    URIParams  = ersip_uri:params(URI),
    maps:is_key(lr, URIParams).

-spec params(route()) -> [route_param()].
params(#route{params = P}) ->
    P.

-spec set_param(Key :: binary(), Value :: binary(), route()) -> route().
set_param(Key, Value, #route{params = Params} = Route)
        when is_binary(Key), is_binary(Value) ->
    Route#route{params = [{Key, Value} | Params]}.

-spec make(iolist()) -> route_set().
make(Binary) ->
    H0 = ersip_hdr:new(<<"Route">>),
    H1 = ersip_hdr:add_value(Binary, H0),
    case parse(H1) of
        {ok, RouteSet} ->
            RouteSet;
        {error, _} = Error  ->
            error(Error)
    end.

-spec parse(ersip_hdr:header()) -> parse_result().
parse(Header) ->
    MaybeRevRouteSet =
        lists:foldl(fun(IORoute, Acc) ->
                            add_to_maybe_route_set(iolist_to_binary(IORoute), Acc)
                    end,
                    {ok, ersip_route_set:new()},
                    ersip_hdr:raw_values(Header)),
    case MaybeRevRouteSet of
        {ok, RevRouteSet} ->
            {ok, ersip_route_set:reverse(RevRouteSet)};
        Error ->
            Error
    end.


-spec build(HeaderName :: binary(), route_set()) -> ersip_hdr:header().
build(HdrName, {route_set, _} = RouteSet) ->
    Hdr = ersip_hdr:new(HdrName),
    ersip_route_set:foldl(
      fun(Route, HdrAcc) ->
              ersip_hdr:add_value(assemble_route(Route), HdrAcc)
      end,
      Hdr,
      RouteSet).

-spec make_route(binary() | ersip_uri:uri()) -> route().
make_route(#uri{} = URI) ->
    #route{display_name = {display_name, []}, uri = URI};
make_route(Bin) when is_binary(Bin) ->
    case parse_route(Bin) of
        {ok, Route} ->
            Route;
        {error, _} = Error ->
            error(Error)
    end.

%%%===================================================================
%%% Helpers
%%%===================================================================

-spec add_to_maybe_route_set(binary(), maybe_rev_route_set()) -> maybe_rev_route_set().
add_to_maybe_route_set(_, {error, _} = Error) ->
    Error;
add_to_maybe_route_set(Bin, {ok, RouteSet}) ->
    case parse_route(Bin) of
        {ok, Route} ->
            {ok, ersip_route_set:add_first(Route, RouteSet)};
        {error, _} = Error ->
            Error
    end.

-spec parse_route(binary()) -> parse_result().
parse_route(Bin) ->
    Parsers = [fun ersip_nameaddr:parse/1,
               fun ersip_parser_aux:trim_lws/1,
               fun parse_route_params/1
              ],
    case ersip_parser_aux:parse_all(Bin, Parsers) of
        {ok, [{DisplayName, URI}, _, ParamsList], <<>>} ->
            {ok,
             #route{display_name = DisplayName,
                    uri          = URI,
                    params       = ParamsList
                   }
            };
        {error, _} = Error ->
            Error
    end.


-spec assemble_route(route()) -> iolist().
assemble_route(#route{} = Route) ->
    #route{display_name = DN,
           uri = URI,
           params = ParamsList
          } = Route,
    [ersip_nameaddr:assemble(DN, URI),
     lists:map(fun({Key, Value}) when is_binary(Value) ->
                       [<<";">>, Key, <<"=">>, Value];
                  ({Key, novalue})  ->
                       [<<";">>, Key]
               end,
               ParamsList)
    ].

-spec parse_route_params(binary()) -> ersip_parser_aux:parse_result([route_param()]).
parse_route_params(<<$;, Bin/binary>>) ->
    parse_route_params(Bin);
parse_route_params(<<>>) ->
    {ok, [], <<>>};
parse_route_params(Bin) ->
    ersip_parser_aux:parse_kvps(fun route_params_validator/2,
                                <<";">>,
                                Bin).

-spec route_params_validator(binary(), binary() | novalue) -> Result when
      Result :: {ok, {binary(), novalue}}
              | {ok, {binary(), binary()}}
              | {error, {invalid_rr_param, binary()}}.
route_params_validator(Key, novalue) ->
    case ersip_parser_aux:check_token(Key) of
        true ->
            {ok, {Key, novalue}};
        false ->
            {error, {invalid_rr_param, Key}}
    end;
route_params_validator(Key, Value) when is_binary(Value) ->
    case ersip_parser_aux:check_token(Key) of
        true ->
            {ok, {Key, Value}};
        false ->
            {error, {invalid_rr_param, Key}}
    end.
