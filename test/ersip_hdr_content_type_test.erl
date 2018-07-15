%%
%% Copyright (c) 2018 Dmitry Poroh
%% All rights reserved.
%% Distributed under the terms of the MIT License. See the LICENSE file.
%%
%% SIP From/to header tests
%%

-module(ersip_hdr_content_type_test).

-include_lib("eunit/include/eunit.hrl").

%%%===================================================================
%%% Cases
%%%===================================================================

parse_test() ->
    ContentType = success_parse_content_type(<<"application/sdp">>),
    ?assertEqual({mime, <<"application">>, <<"sdp">>},
                 ersip_hdr_content_type:mime_type(ContentType)),

    ContentType2 = success_parse_content_type(<<"text/html; charset=ISO-8859-4">>),
    ?assertEqual({mime, <<"text">>, <<"html">>},
                 ersip_hdr_content_type:mime_type(ContentType2)),
    ?assertEqual([{<<"charset">>, <<"ISO-8859-4">>}],
                 ersip_hdr_content_type:params(ContentType2)),

    ContentType3 = success_parse_content_type(<<"text/html; charset=\"ISO-8859-4\"">>),
    ?assertEqual({mime, <<"text">>, <<"html">>},
                 ersip_hdr_content_type:mime_type(ContentType3)),
    ?assertEqual([{<<"charset">>, <<"\"ISO-8859-4\"">>}],
                 ersip_hdr_content_type:params(ContentType3)),

    %% with spaces near SLASH
    ContentType3 = success_parse_content_type(<<"text / html; charset=\"ISO-8859-4\"">>),
    ?assertEqual({mime, <<"text">>, <<"html">>},
                 ersip_hdr_content_type:mime_type(ContentType3)),
    ?assertEqual([{<<"charset">>, <<"\"ISO-8859-4\"">>}],
                 ersip_hdr_content_type:params(ContentType3)),

    EmptyH = ersip_hdr:new(<<"Content-Type">>),
    ?assertMatch({error, _}, ersip_hdr_content_type:parse(EmptyH)),

    MultiH = ersip_hdr:new(<<"Content-Type">>),
    MultiH1 = ersip_hdr:add_value(<<"application/sdp">>, MultiH),
    MultiH2 = ersip_hdr:add_value(<<"text/html">>, MultiH1),
    ?assertMatch({error, _}, ersip_hdr_content_type:parse(MultiH2)).

make_test() ->
    ContentType = success_parse_content_type(<<"text/html; charset=ISO-8859-4">>),
    ?assertEqual(ContentType, ersip_hdr_content_type:make(<<"text/html; charset=ISO-8859-4">>)),
    ?assertError({error, _}, ersip_hdr_content_type:make(<<"text">>)),
    ?assertError({error, _}, ersip_hdr_content_type:make(<<"text/html;charset=,">>)),
    ContentTypeH = create(<<"text/html; charset=ISO-8859-4">>),
    ?assertEqual(ContentType, ersip_hdr_content_type:make(ContentTypeH)),
    ContentTypeH2 = create(<<"text/html; charset=&">>),
    ?assertError({error, _}, ersip_hdr_content_type:make(ContentTypeH2)).

reassemble_test() ->
    reassemble(<<"application/sdp">>),
    reassemble(<<"text/html;charset=\"ISO-8859-4\"">>).

build_test() ->
    ContentTypeH = create(<<"application/sdp">>),
    {ok, ContentType} = ersip_hdr_content_type:parse(ContentTypeH),
    ?assertEqual(ContentTypeH, ersip_hdr_content_type:build(<<"Content-Type">>, ContentType)).


%%%===================================================================
%%% Helpers
%%%===================================================================
create(Bin) ->
    H = ersip_hdr:new(<<"Content-Type">>),
    ersip_hdr:add_value(Bin, H).

success_parse_content_type(Bin) ->
    H = create(Bin),
    {ok, Hdr} = ersip_hdr_content_type:parse(H),
    Hdr.

reassemble(Bin) ->
    ContentType = success_parse_content_type(Bin),
    ?assertEqual(Bin, iolist_to_binary(ersip_hdr_content_type:assemble(ContentType))).
