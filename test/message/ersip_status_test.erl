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
    test_phrase(180, "Ringing"),
    test_phrase(182, "Queued"),
    test_phrase(183, "Session Progress"),
    test_phrase(200, "OK"),
    test_phrase(202, "Accepted"),
    test_phrase(300, "Multiple Choices"),
    test_phrase(301, "Moved Permanently"),
    test_phrase(302, "Moved Temporarily"),
    test_phrase(305, "Use Proxy"),
    test_phrase(380, "Alternative Service"),
    test_phrase(401, "Unauthorized"),
    test_phrase(402, "Payment Required"),
    test_phrase(403, "Forbidden"),
    test_phrase(404, "Not Found"),
    test_phrase(405, "Method Not Allowed"),
    test_phrase(406, "Not Acceptable"),
    test_phrase(407, "Proxy Authentication Required"),
    test_phrase(408, "Request Timeout"),
    test_phrase(410, "Gone"),
    test_phrase(413, "Request Entity Too Large"),
    test_phrase(414, "Request-URI Too Long"),
    test_phrase(415, "Unsupported Media Type"),
    test_phrase(416, "Unsupported URI Scheme"),
    test_phrase(420, "Bad Extension"),
    test_phrase(421, "Extension Required"),
    test_phrase(423, "Interval Too Brief"),
    test_phrase(480, "Temporarily Unavailable"),
    test_phrase(481, "Call/Transaction Does Not Exist"),
    test_phrase(482, "Loop detected"),
    test_phrase(483, "Too many hops"),
    test_phrase(484, "Address Incomplete"),
    test_phrase(485, "Ambiguous"),
    test_phrase(486, "Busy Here"),
    test_phrase(487, "Request Terminated"),
    test_phrase(488, "Not Acceptable Here"),
    test_phrase(491, "Request Pending"),
    test_phrase(493, "Undecipherable"),
    test_phrase(500, "Internal Server Error"),
    test_phrase(501, "Not Implemented"),
    test_phrase(502, "Bad Gateway"),
    test_phrase(503, "Service Unavailable"),
    test_phrase(504, "Server Time-out"),
    test_phrase(505, "Version Not Supported"),
    test_phrase(513, "Message Too Large"),
    test_phrase(600, "Busy Everywhere"),
    test_phrase(603, "Decline"),
    test_phrase(604, "Does Not Exist Anywhere"),
    test_phrase(606, "Not Acceptable"),

    test_phrase(199, "Unknown Status"),
    ok.

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
