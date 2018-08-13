%%
%% Copyright (c) 2018 Dmitry Poroh
%% All rights reserved.
%% Distributed under the terms of the MIT License. See the LICENSE file.
%%
%% SIP message tests
%%

-module(ersip_rfc4475_test).

-include_lib("eunit/include/eunit.hrl").

%%%===================================================================
%%% Cases
%%%===================================================================

% bad() ->
%     [badaspec, baddn, bcast, clerr, escruri, invut, lwsstart, mismatch02, novelsc, regbadct,
%      scalarlg, unkscm, badbranch, badinv01, bext01, cparam01, insuf, ltgtruri, mcl01, multi01, quotbal,
%      regescrt, sdp01, unksm2, baddate, badvers, bigcode, cparam02, inv2543, lwsruri, mismatch01, ncl,
%      regaut01, scalar02, trws, zeromf].
% good() ->
%     [dblreq, esc01, esc02, escnull, intmeth, longreq, lwsdisp, mpart01, noreason, semiuri,
%      transports, unreason, wsinv].

fname(Name) ->
    lists:concat(["test/rfc4475/", Name, ".dat"]).

-define(GOOD(Id),{title(Id), ?_assertMatch({ok, _}, haveto(Id))}).
-define(GOOD_DGRAM(Id),{title(Id), ?_assertMatch({ok, _}, dgram_haveto(Id))}).
-define(BAD(Id),{title( Id), ?_assertMatch({error, _}, haveto(Id))}).
-define(BAD_DGRAM(Id),{title( Id), ?_assertMatch({error, _}, dgram_haveto(Id))}).

parse_rfc4475_good_test_() ->
    [
     ?GOOD(dblreq),
     ?GOOD(esc01),
     ?GOOD(esc02),
     ?GOOD(escnull),
     ?GOOD(intmeth),
     ?GOOD(longreq),
     ?GOOD(lwsdisp),
     ?GOOD(mpart01),
     ?GOOD(noreason),
     ?GOOD(semiuri),
     ?GOOD(transports),
     ?GOOD(unreason),
     ?GOOD(wsinv),
     ?GOOD_DGRAM(inv2543),
     ?GOOD(cparam01),
     ?GOOD(cparam02),
     ?GOOD(regescrt),
     ?GOOD(unkscm),  %% Well-formed message, but unknown scheme for UA
     ?GOOD(unksm2),  %% Good for proxy, bad for registrar...
     ?GOOD(novelsc), %% Well-formed message, but unknown scheme for UA
     ?GOOD(invut),   %% Well-formed message, but unknown content for endpoint
     ?GOOD(sdp01)    %% Well-formed message, UA should reject it because unknown content type
    ].

parse_rfc4475_bad_test_() ->
    [
     ?BAD(badaspec),
     ?BAD(baddn),
%%     ?BAD(bcast),
     ?BAD_DGRAM(clerr),
%%     ?BAD(escruri),
     ?BAD(lwsstart),
     ?BAD(mismatch02),
%%     ?BAD(regbadct),
     ?BAD(scalarlg),
%%     ?BAD(badbranch),
     ?BAD(badinv01),
     ?BAD(bext01),
     ?BAD(insuf),
     ?BAD(ltgtruri),
     ?BAD(mcl01),
     ?BAD(multi01),
     ?BAD(quotbal),
     ?BAD(baddate),
     ?BAD(badvers),
     ?BAD(bigcode),
     ?BAD(lwsruri),
     ?BAD(mismatch01),
     ?BAD(ncl),
%%     ?BAD(regaut01),
     ?BAD(scalar02),
     %%     ?BAD(trws),  %% ignoring because it's acceptable to accept or reject.
     ?BAD(zeromf)
    ].

%%%===================================================================
%%% Helpers
%%%===================================================================
dgram_haveto(Name) ->
    % ?debugVal(Name),
    {ok, SipBin} = file:read_file(fname(Name)),
    P0 = ersip_parser:new_dgram(SipBin),
    haveto1(ersip_parser:parse(P0)).

haveto(Name) ->
    % ?debugVal(Name),
    {ok, SipBin} = file:read_file(fname(Name)),
    P0 = ersip_parser:new(),
    P1 = ersip_parser:add_binary(SipBin, P0),
    haveto1(ersip_parser:parse(P1)).


haveto1({{ok, PMsg}, _P2}) ->
    validation(ersip_sipmsg:parse(PMsg, all));
haveto1({{error,Any}, _}) -> {error, Any}.

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

descr(wsinv)      -> "A Short Tortuous INVITE";
descr(intmeth)    -> "Wide Range of Valid Characters";
descr(esc01)      -> "Valid Use of the % Escaping Mechanism";
descr(escnull)    -> "Escaped Nulls in URIs";
descr(esc02)      -> "Use of % When It Is Not an Escape";
descr(lwsdisp)    -> "Message with No LWS between Display Name and <";
descr(longreq)    -> "Long Values in Header Fields";
descr(dblreq)     -> "Extra Trailing Octets in a UDP Datagram";
descr(semiuri)    -> "Semicolon-Separated Parameters in URI User Part";
descr(transports) -> "Varied and Unknown Transport Types";
descr(mpart01)    -> "Multipart MIME Message";
descr(unreason)   -> "Unusual Reason Phrase";
descr(noreason)   -> "Empty Reason Phrase";
descr(badinv01)   -> "Extraneous Header Field Separators";
descr(clerr)      -> "Content Length Larger Than Message";
descr(ncl)        -> "Negative Content-Length";
descr(scalar02)   -> "Request Scalar Fields with Overlarge Values";
descr(scalarlg)   -> "Response Scalar Fields with Overlarge Values";
descr(quotbal)    -> "Unterminated Quoted String in Display Name";
descr(ltgtruri)   -> "<> Enclosing Request-URI";
descr(lwsruri)    -> "Malformed SIP Request-URI (embedded LWS)";
descr(lwsstart)   -> "Multiple SP Separating Request-Line Elements";
descr(trws)       -> "SP Characters at End of Request-Line";
descr(escruri)    -> "Escaped Headers in SIP Request-URI";
descr(baddate)    -> "Invalid Time Zone in Date Header Field";
descr(regbadct)   -> "Failure to Enclose name-addr URI in <>";
descr(badaspec)   -> "Spaces within addr-spec";
descr(baddn)      -> "Non-token Characters in Display Name";
descr(badvers)    -> "Unknown Protocol Version";
descr(mismatch01) -> "Start Line and CSeq Method Mismatch";
descr(mismatch02) -> "Unknown Method with CSeq Method Mismatch";
descr(bigcode)    -> "Overlarge Response Code";
descr(badbranch)  -> "Missing Transaction Identifier";
descr(insuf)      -> "Missing Required Header Fields";
descr(unkscm)     -> "Request-URI with Unknown Scheme";
descr(novelsc)    -> "Request-URI with Known but Atypical Scheme";
descr(unksm2)     -> "Unknown URI Schemes in Header Fields";
descr(bext01)     -> "Proxy-Require and Require";
descr(invut)      -> "Unknown Content-Type";
descr(regaut01)   -> "Unknown Authorization Scheme";
descr(multi01)    -> "Multiple Values in Single Value Required Fields";
descr(mcl01)      -> "Multiple Content-Length Values";
descr(bcast)      -> "200 OK Response with Broadcast Via Header Field Value";
descr(zeromf)     -> "Max-Forwards of Zero";
descr(cparam01)   -> "REGISTER with a Contact Header Parameter";
descr(cparam02)   -> "REGISTER with a url-parameter";
descr(regescrt)   -> "REGISTER with a URL Escaped Header";
descr(sdp01)      -> "Unacceptable Accept Offering";
descr(inv2543)    -> "INVITE with RFC 2543 Syntax".

