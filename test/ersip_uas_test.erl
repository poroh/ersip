%%
%% Copyright (c) 2018 Dmitry Poroh
%% All rights reserved.
%% Distributed under the terms of the MIT License. See the LICENSE file.
%%
%% UAS tests
%%

-module(ersip_uas_test).

-include_lib("eunit/include/eunit.hrl").

%%%===================================================================
%%% Cases
%%%===================================================================

uas_basic_test() ->
    REGISTERSipMsg = register_request(),
    ProcessResult = ersip_uas:process_request(REGISTERSipMsg, allowed_methods(), #{}),
    ?assertMatch({process, _}, ProcessResult),
    {process, SipMsg} = ProcessResult,
    ?assertEqual(ersip_method:register(), ersip_sipmsg:method(SipMsg)),
    ok.


uas_not_allowed_method_test() ->
    REGISTERSipMsg = register_request(),
    AllowedMethods = ersip_method_set:invite_set(),
    ProcessResult = ersip_uas:process_request(REGISTERSipMsg, AllowedMethods, #{}),
    ?assertMatch({reply, _}, ProcessResult),
    {reply, Resp405} = ProcessResult,
    ?assertEqual(405, ersip_sipmsg:status(Resp405)),
    %% The UAS MUST also add an Allow header field to the 405 (Method
    %% Not Allowed) response.  The Allow header field MUST list the
    %% set of methods supported by the UAS generating the message.
    AllowHdr = ersip_sipmsg:get(allow, Resp405),
    ?assertEqual({allow, AllowedMethods}, AllowHdr),
    ok.

cannot_parse_require_field_test() ->
    REGISTERSipMsg = register_request_bad_require(make_default_source()),
    ProcessResult = ersip_uas:process_request(REGISTERSipMsg, allowed_methods(), #{}),
    ?assertMatch({reply, _}, ProcessResult),
    {reply, Resp400} = ProcessResult,
    ?assertEqual(400, ersip_sipmsg:status(Resp400)),
    ok.

scheme_validation_fail_test() ->
    Options = #{check_scheme => fun(_) -> false end
               },
    REGISTERSipMsg = register_request(),
    ProcessResult = ersip_uas:process_request(REGISTERSipMsg, allowed_methods(), Options),
    ?assertMatch({reply, _}, ProcessResult),
    {reply, Resp416} = ProcessResult,
    ?assertEqual(416, ersip_sipmsg:status(Resp416)),
    ok.

scheme_validation_success_test() ->
    Options = #{check_scheme => fun(S) -> S == {scheme, <<"tel">>} end
               },
    REGISTERSipMsgTEL = register_request_tel_uri(make_default_source()),
    ProcessResultTEL = ersip_uas:process_request(REGISTERSipMsgTEL, allowed_methods(), Options),
    ?assertMatch({process, _}, ProcessResultTEL),
    REGISTERSipMsgSIP = register_request(make_default_source()),
    ProcessResultSIP = ersip_uas:process_request(REGISTERSipMsgSIP, allowed_methods(), Options),
    ?assertMatch({reply, _}, ProcessResultSIP),
    {reply, Resp416} = ProcessResultSIP,
    ?assertEqual(416, ersip_sipmsg:status(Resp416)),
    ok.

scheme_validation_default_test() ->
    Options = #{},
    REGISTERSipMsgTEL = register_request_tel_uri(make_default_source()),
    ProcessResultTEL = ersip_uas:process_request(REGISTERSipMsgTEL, allowed_methods(), Options),
    ?assertMatch({reply, _}, ProcessResultTEL),
    {reply, Resp416} = ProcessResultTEL,
    ?assertEqual(416, ersip_sipmsg:status(Resp416)),

    REGISTERSipMsgSIPS = register_request_sips_uri(make_default_source()),
    ProcessResultSIPS = ersip_uas:process_request(REGISTERSipMsgSIPS, allowed_methods(), Options),
    ?assertMatch({process, _}, ProcessResultSIPS),
    ok.

extension_is_not_supported_test() ->
    Options = #{},
    REGISTERSipMsgGRUU = register_request_gruu(make_default_source()),
    ProcessResult = ersip_uas:process_request(REGISTERSipMsgGRUU, allowed_methods(), Options),
    ?assertMatch({reply, _}, ProcessResult),
    {reply, Resp420} = ProcessResult,
    ?assertEqual(420, ersip_sipmsg:status(Resp420)),
    Unsupported = ersip_hdr_opttag_list:from_list([ersip_option_tag:make(<<"gruu">>)]),
    ?assertEqual(Unsupported, ersip_sipmsg:get(unsupported, Resp420)),
    ok.

extension_is_not_supported_not_intersection_test() ->
    Supported = ersip_hdr_opttag_list:from_list([ersip_option_tag:make(<<"gin">>)
                                                ]),
    Options = #{supported => Supported
               },
    REGISTERSipMsgGRUU = register_request_gin_gruu(make_default_source()),
    ProcessResult = ersip_uas:process_request(REGISTERSipMsgGRUU, allowed_methods(), Options),
    ?assertMatch({reply, _}, ProcessResult),
    {reply, Resp420} = ProcessResult,
    ?assertEqual(420, ersip_sipmsg:status(Resp420)),
    Unsupported = ersip_hdr_opttag_list:from_list([ersip_option_tag:make(<<"gruu">>)]),
    ?assertEqual(Unsupported, ersip_sipmsg:get(unsupported, Resp420)),
    ok.

to_tag_options_passing_test() ->
    ToTag = {tag, <<"asdjkwed">>},
    Options = #{to_tag => ToTag
               },
    REGISTERSipMsg = register_request(),
    ProcessResult = ersip_uas:process_request(REGISTERSipMsg, ersip_method_set:invite_set(), Options),
    ?assertMatch({reply, _}, ProcessResult),
    {reply, Resp405} = ProcessResult,
    To = ersip_sipmsg:get(to, Resp405),
    ?assertEqual(ToTag, ersip_hdr_fromto:tag(To)),
    ok.

extension_is_supported_test() ->
    Supported = ersip_hdr_opttag_list:from_list([ersip_option_tag:make(<<"gin">>),
                                                 ersip_option_tag:make(<<"gruu">>)
                                                ]),
    Options = #{supported => Supported
               },
    REGISTERSipMsgGRUU = register_request_gruu(make_default_source()),
    ProcessResult = ersip_uas:process_request(REGISTERSipMsgGRUU, allowed_methods(), Options),
    ?assertMatch({process, _}, ProcessResult),
    ok.


%%%===================================================================
%%% Helpers
%%%===================================================================


-define(crlf, "\r\n").

allowed_methods() ->
    ersip_method_set:new([ersip_method:register()]).

register_request() ->
    Msg = register_request_bin(),
    create_msg(Msg, make_default_source()).

register_request(Source) ->
    Msg = register_request_bin(),
    create_sipmsg(Msg, Source).

register_request_bad_require(Source) ->
    Msg = register_request_bad_require_bin(),
    create_sipmsg(Msg, Source, []).

register_request_tel_uri(Source) ->
    Msg = register_request_tel_uri_bin(),
    create_sipmsg(Msg, Source, []).

register_request_sips_uri(Source) ->
    Msg = register_request_sips_uri_bin(),
    create_sipmsg(Msg, Source, []).

register_request_gruu(Source) ->
    Msg = register_request_gruu_bin(),
    create_sipmsg(Msg, Source, all).

register_request_gin_gruu(Source) ->
    Msg = register_request_gin_gruu_bin(),
    create_sipmsg(Msg, Source, all).

register_request_bin() ->
    <<"REGISTER sip:192.168.100.11:5060 SIP/2.0" ?crlf
      "Via: SIP/2.0/UDP 192.168.100.11:5090;branch=z9hG4bK*77yCNomtXelRpoCGdCfE" ?crlf
      "Via: SIP/2.0/UDP 192.168.100.11:5070;rport;branch=z9hG4bK785703841" ?crlf
      "To: <sip:1000@192.168.100.11:5060>" ?crlf
      "From: <sip:1000@192.168.100.11:5060>;tag=1452599670" ?crlf
      "Call-ID: 1197534344" ?crlf
      "CSeq: 4 REGISTER" ?crlf
      "Max-Forwards: 69" ?crlf
      "Expires: 3600" ?crlf
      "Content-Length: 0" ?crlf
      "Contact: <sip:1000@192.168.100.11:5070;line=69210a2e715cee1>" ?crlf
      "Record-Route: <sip:192.168.100.11:5090;lr>" ?crlf
      "User-Agent: Linphone/3.6.1 (eXosip2/4.1.0)" ?crlf
      ?crlf>>.

register_request_bad_require_bin() ->
    <<"REGISTER sip:192.168.100.11:5060 SIP/2.0" ?crlf
      "Via: SIP/2.0/UDP 192.168.100.11:5090;branch=z9hG4bK*77yCNomtXelRpoCGdCfE" ?crlf
      "Via: SIP/2.0/UDP 192.168.100.11:5070;rport;branch=z9hG4bK785703841" ?crlf
      "To: <sip:1000@192.168.100.11:5060>" ?crlf
      "From: <sip:1000@192.168.100.11:5060>;tag=1452599670" ?crlf
      "Call-ID: 1197534344" ?crlf
      "CSeq: 4 REGISTER" ?crlf
      "Max-Forwards: 69" ?crlf
      "Expires: 3600" ?crlf
      "Require: ?" ?crlf
      "Content-Length: 0" ?crlf
      "Contact: <sip:1000@192.168.100.11:5070;line=69210a2e715cee1>" ?crlf
      "Record-Route: <sip:192.168.100.11:5090;lr>" ?crlf
      "User-Agent: Linphone/3.6.1 (eXosip2/4.1.0)" ?crlf
      ?crlf>>.

register_request_gruu_bin() ->
    <<"REGISTER sip:192.168.100.11:5060 SIP/2.0" ?crlf
      "Via: SIP/2.0/UDP 192.168.100.11:5090;branch=z9hG4bK*77yCNomtXelRpoCGdCfE" ?crlf
      "Via: SIP/2.0/UDP 192.168.100.11:5070;rport;branch=z9hG4bK785703841" ?crlf
      "To: <sip:1000@192.168.100.11:5060>" ?crlf
      "From: <sip:1000@192.168.100.11:5060>;tag=1452599670" ?crlf
      "Call-ID: 1197534344" ?crlf
      "CSeq: 4 REGISTER" ?crlf
      "Max-Forwards: 69" ?crlf
      "Expires: 3600" ?crlf
      "Require: gruu" ?crlf
      "Content-Length: 0" ?crlf
      "Contact: <sip:1000@192.168.100.11:5070;line=69210a2e715cee1>" ?crlf
      "Record-Route: <sip:192.168.100.11:5090;lr>" ?crlf
      "User-Agent: Linphone/3.6.1 (eXosip2/4.1.0)" ?crlf
      ?crlf>>.

register_request_gin_gruu_bin() ->
    <<"REGISTER sip:192.168.100.11:5060 SIP/2.0" ?crlf
      "Via: SIP/2.0/UDP 192.168.100.11:5090;branch=z9hG4bK*77yCNomtXelRpoCGdCfE" ?crlf
      "Via: SIP/2.0/UDP 192.168.100.11:5070;rport;branch=z9hG4bK785703841" ?crlf
      "To: <sip:1000@192.168.100.11:5060>" ?crlf
      "From: <sip:1000@192.168.100.11:5060>;tag=1452599670" ?crlf
      "Call-ID: 1197534344" ?crlf
      "CSeq: 4 REGISTER" ?crlf
      "Max-Forwards: 69" ?crlf
      "Expires: 3600" ?crlf
      "Require: gin,gruu" ?crlf
      "Content-Length: 0" ?crlf
      "Contact: <sip:1000@192.168.100.11:5070;line=69210a2e715cee1>" ?crlf
      "Record-Route: <sip:192.168.100.11:5090;lr>" ?crlf
      "User-Agent: Linphone/3.6.1 (eXosip2/4.1.0)" ?crlf
      ?crlf>>.

register_request_tel_uri_bin() ->
    <<"REGISTER tel:+111 SIP/2.0" ?crlf
      "Via: SIP/2.0/UDP 192.168.100.11:5090;branch=z9hG4bK*77yCNomtXelRpoCGdCfE" ?crlf
      "Via: SIP/2.0/UDP 192.168.100.11:5070;rport;branch=z9hG4bK785703841" ?crlf
      "To: <sip:1000@192.168.100.11:5060>" ?crlf
      "From: <sip:1000@192.168.100.11:5060>;tag=1452599670" ?crlf
      "Call-ID: 1197534344" ?crlf
      "CSeq: 4 REGISTER" ?crlf
      "Max-Forwards: 69" ?crlf
      "Expires: 3600" ?crlf
      "Content-Length: 0" ?crlf
      "Contact: <sip:1000@192.168.100.11:5070;line=69210a2e715cee1>" ?crlf
      "Record-Route: <sip:192.168.100.11:5090;lr>" ?crlf
      "User-Agent: Linphone/3.6.1 (eXosip2/4.1.0)" ?crlf
      ?crlf>>.

register_request_sips_uri_bin() ->
    <<"REGISTER sips:192.168.100.11:5061 SIP/2.0" ?crlf
      "Via: SIP/2.0/UDP 192.168.100.11:5090;branch=z9hG4bK*77yCNomtXelRpoCGdCfE" ?crlf
      "Via: SIP/2.0/UDP 192.168.100.11:5070;rport;branch=z9hG4bK785703841" ?crlf
      "To: <sip:1000@192.168.100.11:5060>" ?crlf
      "From: <sip:1000@192.168.100.11:5060>;tag=1452599670" ?crlf
      "Call-ID: 1197534344" ?crlf
      "CSeq: 4 REGISTER" ?crlf
      "Max-Forwards: 69" ?crlf
      "Expires: 3600" ?crlf
      "Content-Length: 0" ?crlf
      "Contact: <sip:1000@192.168.100.11:5070;line=69210a2e715cee1>" ?crlf
      "Record-Route: <sip:192.168.100.11:5090;lr>" ?crlf
      "User-Agent: Linphone/3.6.1 (eXosip2/4.1.0)" ?crlf
      ?crlf>>.

make_default_source() ->
    tcp_source(default_peer()).

default_peer() ->
    {{127, 0, 0, 1}, 5060}.

tcp_source(Peer) ->
    ersip_source:new(Peer, ersip_transport:tcp(), undefined).

create_sipmsg(Msg, Source) when is_binary(Msg) ->
    create_sipmsg(Msg, Source, all).

create_sipmsg(Msg, Source, HeadersToParse) when is_binary(Msg) ->
    P  = ersip_parser:new_dgram(Msg),
    {{ok, PMsg}, _P2} = ersip_parser:parse(P),
    PMsg1 = ersip_msg:set_source(Source, PMsg),
    {ok, SipMsg} = ersip_sipmsg:parse(PMsg1, HeadersToParse),
    SipMsg.

create_msg(Msg, Source) when is_binary(Msg) ->
    P  = ersip_parser:new_dgram(Msg),
    {{ok, PMsg}, _P2} = ersip_parser:parse(P),
    ersip_msg:set_source(Source, PMsg).
