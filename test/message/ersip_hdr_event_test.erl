%%
%% Copyright (c) 2019 Dmitry Poroh
%% All rights reserved.
%% Distributed under the terms of the MIT License. See the LICENSE file.
%%
%% SIP Event tests
%%

-module(ersip_hdr_event_test).

-include_lib("eunit/include/eunit.hrl").

%%===================================================================
%% Cases
%%===================================================================

rebuild_test() ->
    rebuild(<<"dialog">>),
    rebuild(<<"Dialog">>),
    rebuild(<<"dialog;id=1">>),
    rebuild(<<"dialog;id=x-some-my-id">>),
    rebuild(<<"dialog;Id=x-some-my-id">>),
    rebuild(<<"Dialog;id=x-some-my-id">>),
    ok.

parse_error_test() ->
    parse_error(<<"dialog;id=\"@\"">>),
    parse_error(<<"dialog;id=@">>),
    parse_error(<<"dialog;id=1,*">>),
    parse_error(<<"dialog;a@b=40">>),
    parse_error(<<"dialog@x">>),
    parse_error(<<"dialog;id=\"a@b\"">>),
    parse_error(<<"dialog;?=$">>),
    parse_error(<<"dialog;?">>),
    parse_error(<<"dialog;;;;">>),
    ok.

parse_test() ->
    ?assertEqual({unknown_event, <<"dialog">>}, type(<<"dialog;id=1">>)),
    ?assertEqual({unknown_event, <<"dialog">>}, type(<<"Dialog;id=1">>)),
    ?assertEqual(<<"dialog">>, type_bin(<<"dialog;id=1">>)),
    ?assertEqual(<<"dialog">>, type_bin(<<"Dialog;id=1">>)),
    ?assertEqual(<<"timeout">>, event_id(<<"dialog;id=timeout">>)),
    ?assertEqual(<<"timeout">>, event_id(<<"dialog;id=Timeout">>)),
    ?assertEqual(undefined, event_id(<<"dialog">>)),

    ?assertEqual(<<"timeout">>, param(<<"id">>, <<"dialog;id=timeout">>)),
    ?assertEqual(<<"1">>, param(<<"my-param">>, <<"dialog;my-param=1">>)),
    ok.

make_error_test() ->
    ?assertError({invalid_event, _}, ersip_hdr_event:make(<<"dialog;id=@">>)),
    ?assertError({invalid_event, _}, ersip_hdr_event:make(<<"dialog;@=1">>)),
    ok.

set_param_test() ->
    EventId = ersip_hdr_event:make(<<"dialog;id=aaa">>),
    EventIdBBB = ersip_hdr_event:set_param(<<"id">>, <<"BBB">>, EventId),
    ?assertEqual(<<"bbb">>, ersip_hdr_event:id(EventIdBBB, undefined)),
    ?assertEqual(<<"dialog;id=BBB">>, ersip_hdr_event:assemble_bin(EventIdBBB)),

    EventOther55 = ersip_hdr_event:make(<<"dialog;some=55">>),
    EventOther99 = ersip_hdr_event:set_param(<<"some">>, <<"99">>, EventOther55),
    ?assertEqual({ok, <<"99">>}, ersip_hdr_event:param(<<"some">>, EventOther99)),
    ?assertEqual(<<"dialog;some=99">>, ersip_hdr_event:assemble_bin(EventOther99)),

    ok.

set_param_error_test() ->
    Event = ersip_hdr_event:make(<<"dialog;id=deactivated">>),
    ?assertError({invalid_event_id, _},  ersip_hdr_event:set_param(<<"id">>, <<"@">>, Event)),
    ?assertError({invalid_event_id, _},  ersip_hdr_event:set_param(<<"id">>, <<"a@b">>, Event)),
    ?assertError({invalid_param, _}, ersip_hdr_event:set_param(<<"@">>, <<"Value">>, Event)),
    ?assertError({invalid_param, _}, ersip_hdr_event:set_param(<<"@">>, <<>>, Event)),
    ok.


build_test() ->
    EventH = create(<<"dialog;id=1">>),
    {ok, Event} = ersip_hdr_event:parse(EventH),
    EventHValues = [iolist_to_binary(IOListVal) || IOListVal <- ersip_hdr:raw_values(EventH)],
    BuiltEventH = ersip_hdr_event:build(<<"Event">>, Event),
    BuiltEventHValues = [iolist_to_binary(IOListVal) || IOListVal <- ersip_hdr:raw_values(BuiltEventH)],
    ?assertEqual(EventHValues, BuiltEventHValues),

    EmptyH = ersip_hdr:new(<<"Event">>),
    ?assertEqual({error, no_event}, ersip_hdr_event:parse(EmptyH)),
    ok.

assemble_bin_test() ->
    Event = ersip_hdr_event:make(<<"dialog;id=1">>),
    ?assertEqual(<<"dialog;id=1">>, ersip_hdr_event:assemble_bin(Event)),
    ok.

%%===================================================================
%% Helpers
%%===================================================================
create(Bin) ->
    H = ersip_hdr:new(<<"Event">>),
    ersip_hdr:add_value(Bin, H).

rebuild(Bin) ->
    Event = ersip_hdr_event:make(Bin),
    EventBin = iolist_to_binary(ersip_hdr_event:assemble(Event)),
    {ok, Event1} = ersip_hdr_event:parse(EventBin),
    ?assertEqual(Event, Event1).

parse_error(Bin) ->
    ?assertMatch({error, {invalid_event, _}}, ersip_hdr_event:parse(Bin)).

type(Bin) ->
    Event = ersip_hdr_event:make(Bin),
    ersip_hdr_event:type(Event).

type_bin(Bin) ->
    Event = ersip_hdr_event:make(Bin),
    ersip_hdr_event:type_bin(Event).

event_id(Bin) ->
    Event = ersip_hdr_event:make(Bin),
    ersip_hdr_event:id(Event, undefined).

param(ParamName, Bin) ->
    Event = ersip_hdr_event:make(Bin),
    {ok, Value} = ersip_hdr_event:param(ParamName, Event),
    Value.
