%%
%% Copyright (c) 2017 Dmitry Poroh
%% All rights reserved.
%% Distributed under the terms of the MIT License. See the LICENSE file.
%%
%% Status codes tests
%%

-module(ersip_status_test).

-include_lib("eunit/include/eunit.hrl").


%%%===================================================================
%%% Cases
%%%===================================================================

response_type_test() ->
    ?assertEqual(provisional, ersip_status:response_type(100)),
    ?assertEqual(provisional, ersip_status:response_type(199)),
    ?assertEqual(final, ersip_status:response_type(200)),
    ?assertEqual(final, ersip_status:response_type(299)),
    ?assertEqual(final, ersip_status:response_type(300)),
    ?assertEqual(final, ersip_status:response_type(399)),
    ?assertEqual(final, ersip_status:response_type(400)),
    ?assertEqual(final, ersip_status:response_type(499)),
    ?assertEqual(final, ersip_status:response_type(500)),
    ?assertEqual(final, ersip_status:response_type(599)),
    ?assertEqual(final, ersip_status:response_type(600)),
    ?assertEqual(final, ersip_status:response_type(699)),
    ?assertException(error, function_clause, ersip_status:response_type(99)),
    ?assertException(error, function_clause, ersip_status:response_type(700)),
    ?assertException(error, function_clause, ersip_status:response_type(a)).

reason_text_test() ->
    test_phrase(100, "Trying"),
    test_phrase(404, "Not Found"),
    test_phrase(199, "Unknown Status").

bad_request_reason_test() ->
    test_bad_request_reason("Bad Request",  {error, some_error}),
    test_bad_request_reason("Max-Forwards", {error, {header_error,{maxforwards,some}}}),
    ok.

%%%===================================================================
%%% Helpers
%%%===================================================================

test_phrase(Code, Text) ->
    TextBin = list_to_binary(Text),
    Phrase  = ersip_status:reason_phrase(Code),
    ?assertEqual(TextBin, Phrase).

test_bad_request_reason(MatchText, Error) ->
    Reason = ersip_bin:to_lower(ersip_status:bad_request_reason(Error)),
    ?assert(binary:match(Reason, ersip_bin:to_lower(iolist_to_binary(MatchText))) =/= nomatch).
