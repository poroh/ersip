%%
%% Copyright (c) 2018 Dmitry Poroh
%% All rights reserved.
%% Distributed under the terms of the MIT License. See the LICENSE file.
%%
%% SDP attributes test
%%

-module(ersip_sdp_attr_test).

-include_lib("eunit/include/eunit.hrl").

%%%===================================================================
%%% Cases
%%%===================================================================

-define(crlf, "\r\n").

parse_test() ->
    ?assertEqual({ok, [<<"sendrecv">>], <<>>}, ersip_sdp_attr:parse(<<"a=sendrecv" ?crlf>>)),
    ?assertEqual({ok, [{<<"x">>, <<"y">>}], <<>>}, ersip_sdp_attr:parse(<<"a=x:y" ?crlf>>)),
    ?assertEqual({ok, [], <<>>}, ersip_sdp_attr:parse(<<>>)),
    ok.

parse_error_test() ->
    ?assertMatch({error, {invalid_attr, _}}, ersip_sdp_attr:parse(<<"a=sendrecv">>)),
    ?assertMatch({error, {invalid_attr, _}}, ersip_sdp_attr:parse(<<"a=@:x">>)),
    ok.

