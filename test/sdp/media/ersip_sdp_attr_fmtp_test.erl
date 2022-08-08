%%
%% Copyright (c) 2020 Dmitry Poroh
%% All rights reserved.
%% Distributed under the terms of the MIT License. See the LICENSE file.
%%
%% SDP media attribute fmtp
%%

-module(ersip_sdp_attr_fmtp_test).

-include_lib("eunit/include/eunit.hrl").

%%%===================================================================
%%% Cases
%%%===================================================================

parse_test() ->
    Fmtp0 = <<"101 0-15">>,
    {ok, Fmtpmap0, <<>>} = ersip_sdp_attr_fmtp:parse(Fmtp0),

    ?assertEqual(101, ersip_sdp_attr_fmtp:format(Fmtpmap0)),
    ?assertEqual(<<"0-15">>, ersip_sdp_attr_fmtp:params(Fmtpmap0)),

    Fmtp0 = ersip_sdp_attr_fmtp:assemble_bin(Fmtpmap0),

    ok.

parse_error_test() ->
    ?assertEqual(
        {error,{invalid_fmtp,{invalid_integer,<<"aaa 0-15">>}}},
        ersip_sdp_attr_fmtp:parse(<<"aaa 0-15">>)
    ),
    ok.


