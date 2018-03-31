%%
%% Copyright (c) 2018 Dmitry Poroh
%% All rights reserved.
%% Distributed under the terms of the MIT License. See the LICENSE file.
%%
%% iolist tests
%%

-module(ersip_hdr_route_test).

-include_lib("eunit/include/eunit.hrl").

%%%===================================================================
%%% Cases
%%%===================================================================

topmost_route_test() ->
    Route = topmost_route(<<"<sip:alice@atlanta.com>,<sip:carol@chicago.com>,<sip:bob@biloxi.com>">>),
    ?assertEqual(ersip_uri:make(<<"sip:alice@atlanta.com">>), ersip_hdr_route:uri(Route)),
    RouteWithExt = topmost_route(<<"<sip:alice@atlanta.com>;extension=1">>),
    ?assertEqual(ersip_uri:make(<<"sip:alice@atlanta.com">>), ersip_hdr_route:uri(RouteWithExt)),
    ?assertEqual([ { <<"extension">>, <<"1">> } ], ersip_hdr_route:params(RouteWithExt)),
    RouteWithLR = topmost_route(<<"<sip:alice@atlanta.com;lr>">>),
    URIWithLR = ersip_hdr_route:uri(RouteWithLR),
    ?assertEqual(#{ lr => true }, ersip_uri:params(URIWithLR)),
    ok.

bad_topmost_test() ->
    H = ersip_hdr:new(<<"Route">>),
    H1 = ersip_hdr:add_value(<<"?">>, H),
    ?assertMatch({ error, _ }, ersip_hdr_route:parse(H1)).

bad_middle_test() ->
    H = ersip_hdr:new(<<"Route">>),
    H1 = ersip_hdr:add_value(<<"<sip:alice@atlanta.com>,?,<sip:bob@biloxi.com>">>, H),
    ?assertMatch({ error, _ }, ersip_hdr_route:parse(H1)).

empty_route_test() ->
    H = ersip_hdr:new(<<"Route">>),
    Empty = ersip_route_set:new(),
    ?assertEqual(true, ersip_route_set:is_empty(Empty)),
    ?assertEqual({ ok,  Empty}, ersip_hdr_route:parse(H)).

%%%===================================================================
%%% Helpers
%%%===================================================================

create(RouteBin) ->
    HRoute = ersip_hdr:new(<<"Route">>),
    ersip_hdr:add_value(RouteBin, HRoute). 

topmost_route(RouteBin) ->
    HRoute = create(RouteBin),
    case ersip_hdr_route:parse(HRoute) of
        { ok, RouteSet } -> ersip_route_set:first(RouteSet);
        Error ->
            error(Error)
    end.
