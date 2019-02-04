%%
%% Copyright (c) 2018 Dmitry Poroh
%% All rights reserved.
%% Distributed under the terms of the MIT License. See the LICENSE file.
%%
%% CallID tests
%%

-module(ersip_hdr_callid_test).

-include_lib("eunit/include/eunit.hrl").

%%%===================================================================
%%% Cases
%%%===================================================================

parse_test() ->
    WordChars =
        << "-"  "."  "!"  "%"  "*"
           "_"  "+"  "`"  "'"  "~"
           "("  ")"  "<"  ">"
           ":"  "\\"  "\""
           "/"  "["  "]"  "?"
           "{"  "}"
        >>,
    AlphaNum = iolist_to_binary(lists:seq($A, $Z)
                                ++ lists:seq($a, $z)
                                ++ lists:seq($0, $9)),
    parse_success(<<"a@b">>),
    parse_success(<<WordChars/binary, AlphaNum/binary, "@",
                    AlphaNum/binary, WordChars/binary>>),
%    parse_success(<<WordChars/binary, AlphaNum/binary, "@",
%                    AlphaNum/binary, "@", WordChars/binary>>),
    parse_success(<<AlphaNum/binary, WordChars/binary>>),
    parse_fail(<<",">>),
    parse_fail(<<>>),
    parse_fail(<<"@alfa">>),
    parse_fail(<<"alfa@">>),
    parse_fail(<<"alfa@beta@gamma">>),
    parse_fail(<<"a@@b">>),
    parse_fail(<<"a@b,c@d">>).

make_test() ->
    ?assertError({error, _}, ersip_hdr_callid:make(<<"a@@b">>)),
    H = create_header(<<"a@@b">>),
    ?assertError({error, _}, ersip_hdr_callid:make(H)),
    H1 = create_header(<<"a@b">>),
    ?assertEqual(make(<<"a@b">>), ersip_hdr_callid:make(H1)),
    EmptyH1 = ersip_hdr:new(<<"Call-ID">>),
    ?assertError({error, no_callid}, ersip_hdr_callid:make(EmptyH1)).

make_key_test() ->
    ?assertEqual(make(<<"a@b">>), ersip_hdr_callid:make_key(make(<<"a@b">>))).

reassemble_test() ->
    reassemble(<<"a@b">>),
    reassemble(<<"a">>),
    reassemble(<<"abcdef">>).

assemble_bin_test() ->
    ?assertEqual(<<"a@b">>, ersip_hdr_callid:assemble_bin(ersip_hdr_callid:make(<<"a@b">>))),
    ok.

build_test() ->
    CallIdH = create_header(<<"a@b">>),
    {ok, CallId} = ersip_hdr_callid:parse(CallIdH),
    ?assertEqual(CallIdH, ersip_hdr_callid:build(<<"Call-ID">>, CallId)).


%%%===================================================================
%%% Helpers
%%%===================================================================

make(Bin) ->
    ersip_hdr_callid:make(Bin).

create_header(Bin) ->
    CallIDH = ersip_hdr:new(<<"Call-ID">>),
    ersip_hdr:add_value(Bin, CallIDH).

parse_call_id(Bin) ->
    CallIDH = create_header(Bin),
    ersip_hdr_callid:parse(CallIDH).

parse_success(Bin) ->
    ?assertEqual({ok, make(Bin)}, parse_call_id(Bin)).

parse_fail(Bin) ->
    ?assertMatch({error, _}, parse_call_id(Bin)).

reassemble(Bin) ->
    CallId = make(Bin),
    ?assertEqual(Bin, iolist_to_binary(ersip_hdr_callid:assemble(CallId))).
