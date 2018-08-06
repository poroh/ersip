%%
%% Copyright (c) 2018 Dmitry Poroh
%% All rights reserved.
%% Distributed under the terms of the MIT License. See the LICENSE file.
%%
%% SIP message tests
%%

-module(ersip_rfc5118_test).

-include_lib("eunit/include/eunit.hrl").

%%%===================================================================
%%% Cases
%%%===================================================================
% bad() -> [
% 'ipv6-bad',
% 'ipv6-bug-abnf-3-colons',
% 'port-ambiguous'
%          ].

% good() -> [
% 'ipv6-good',
% 'ipv4-mapped-ipv6',
% 'ipv6-correct-abnf-2-colons',
% 'port-unambiguous',
% 'via-received-param-with-delim',
% 'mult-ip-in-header',
% 'mult-ip-in-sdp',
% 'ipv6-in-sdp',
% 'via-received-param-no-delim'

%           ].


fname(Name) ->
    lists:concat(["test/rfc5118/", Name]).

-define(GOOD(Id),{title(Id), ?_assertMatch({ok, _}, haveto(Id))}).
-define(BAD(Id),{title( Id), ?_assertMatch({error, _}, haveto(Id))}).

parse_rfc5118_good_test_() ->
    [
     ?GOOD('ipv6-good'),
     ?GOOD('ipv6-correct-abnf-2-colons'),
     ?GOOD('port-unambiguous'),
     ?GOOD('via-received-param-with-delim'),
     ?GOOD('mult-ip-in-sdp'),
     ?GOOD('ipv6-in-sdp'),
     ?GOOD('mult-ip-in-header'),
     ?GOOD('ipv4-mapped-ipv6'),
     ?GOOD('via-received-param-no-delim'),
     ?GOOD('port-ambiguous') %% This is precaution for assemble of the message, message is well-formed for parsers.
    ].

parse_rfc5118_bad_test_() ->
    [
     ?BAD('ipv6-bad'),
     ?BAD('ipv6-bug-abnf-3-colons')
    ].

%%%===================================================================
%%% Helpers
%%%===================================================================
haveto(Name) ->
    % ?debugVal(Name),
    {ok, SipBin} = file:read_file(fname(Name)),
    P = ersip_parser:new_dgram(SipBin),
    haveto1(ersip_parser:parse(P)).

haveto1({{ok, PMsg}, _P2}) ->
    validation(ersip_sipmsg:parse(PMsg, all));
haveto1({{error,Any}, _}) -> {error, Any};
haveto1({mor_data, _}) -> {error, more_data}.

validation({error, _} = R) -> R;
validation({ok, Msg}) ->
    case ersip_sipmsg:type(Msg) of
        request ->
            case ersip_proxy_common:request_validation(Msg, #{to_tag => {tag, <<"12345">>}}) of
                {ok, _} = N -> N;
                {reply, Msg1} ->
                    case ersip_sipmsg:status(Msg1) of
                        N when N >299 -> {error, N};
                        N -> {ok, N}
                    end
            end;
        response ->
            case ersip_sipmsg:status(Msg) of
                N when N >299 -> {error, N};
                N -> {ok, N}
            end
    end.

title(Id) -> lists:concat([Id,", ", descr(Id)]).

descr('ipv6-bad') -> "Invalid SIP Message with an IPv6 Reference";
descr('ipv6-bug-abnf-3-colons') -> "IPv6 Reference Bug in RFC 3261 ABNF";
descr('port-ambiguous') -> "Port Ambiguous in a SIP URI";
descr('ipv6-good') -> "Valid SIP Message with an IPv6 Reference";
descr('ipv4-mapped-ipv6') -> "IPv4-Mapped IPv6 Addresses";
descr('ipv6-correct-abnf-2-colons') -> " correct syntax for the IPv6 reference in the RURI";
descr('port-unambiguous') -> "Port Unambiguous in a SIP URI";
descr('via-received-param-with-delim') -> "IPv6 Reference Delimiters in Via Header with delims";
descr('mult-ip-in-header') -> "Multiple IP Addresses in SIP Headers";
descr('mult-ip-in-sdp') -> "Multiple IP Addresses in SDP";
descr('ipv6-in-sdp') -> "SIP Request with IPv6 Addresses in SDP";
descr('via-received-param-no-delim') -> "IPv6 Reference Delimiters in Via Header, no delims".

