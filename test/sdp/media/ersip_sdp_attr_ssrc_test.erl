%%
%% Copyright (c) 2020 Dmitry Poroh
%% All rights reserved.
%% Distributed under the terms of the MIT License. See the LICENSE file.
%%
%% SDP media attribute ssrc
%%

-module(ersip_sdp_attr_ssrc_test).

-include_lib("eunit/include/eunit.hrl").

%%%===================================================================
%%% Cases
%%%===================================================================

parse_test() ->
    Ssrc0 = <<"1231564868 cname:/M54tYYRX0rCr0kOK3JBMA==">>,
    {ok, SsrcMap0, <<>>} = ersip_sdp_attr_ssrc:parse(Ssrc0),

    1231564868 = ersip_sdp_attr_ssrc:id(SsrcMap0),
    {<<"cname">>, <<"/M54tYYRX0rCr0kOK3JBMA==">>} = ersip_sdp_attr_ssrc:attr(SsrcMap0),

    Ssrc0 = ersip_sdp_attr_ssrc:assemble_bin(SsrcMap0),

    Ssrc1 = <<"1231564868 cname">>,
    {ok, SsrcMap1, <<>>} = ersip_sdp_attr_ssrc:parse(Ssrc1),

    1231564868 = ersip_sdp_attr_ssrc:id(SsrcMap1),
    <<"cname">> = ersip_sdp_attr_ssrc:attr(SsrcMap1),

    Ssrc1 = ersip_sdp_attr_ssrc:assemble_bin(SsrcMap1),

    ok.

parse_error_test() ->
    ?assertEqual(
        {error, {invalid_ssrc, {invalid_integer, <<"asd cname:/M54tYYRX0rCr0kOK3JBMA==">>}}},
        ersip_sdp_attr_ssrc:parse(<<"asd cname:/M54tYYRX0rCr0kOK3JBMA==">>)
    ),
    ok.


