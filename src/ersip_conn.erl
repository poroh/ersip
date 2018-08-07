%%
%% Copyright (c) 2018 Dmitry Poroh
%% All rights reserved.
%% Distributed under the terms of the MIT License. See the LICENSE file.
%%
%% SIP Connection
%%
%% Describes one SIP connection (from one source)
%%

-module(ersip_conn).

-export([new/6,
         conn_data/2,
         add_via/3,
         take_via/2
        ]).
-export_type([sip_conn/0]).

%%%===================================================================
%%% Types
%%%===================================================================

-type result() :: {sip_conn(), [ersip_conn_se:side_effect()]}.
-record(sip_conn, {
          local_addr  :: {ersip_host:host(), inet:port_number()},
          remote_addr :: {ersip_host:host(), inet:port_number()},
          transport   :: ersip_transport:transport(),
          options     :: options(),
          parser      :: ersip_parser:data() | undefined
         }).
-type sip_conn() :: #sip_conn{}.
-type options()  :: map().
-type maybe_message() :: {ok, ersip_msg:message()}
                       | {error, term()}.
-type maybe_via() :: {ok, ersip_hdr_via:via()}
                   | {error, term()}.

%%%===================================================================
%%% API
%%%===================================================================

-spec new(LocalAddr, LocalPort, RemoteAddr, RemotePort, SIPTransport, Options) -> sip_conn() when
      LocalAddr    :: inet:ip_address(),
      LocalPort    :: inet:port_number(),
      RemoteAddr   :: inet:ip_address(),
      RemotePort   :: inet:port_number(),
      SIPTransport :: ersip_transport:transport(),
      Options      :: options().
new(LocalAddr, LocalPort, RemoteAddr, RemotePort, SIPTransport, Options) ->
    ParserOptions = maps:get(parser, Options, #{}),
    IsDgram       = ersip_transport:is_datagram(SIPTransport),
    #sip_conn{
       local_addr  = {ersip_host:make(LocalAddr),  LocalPort},
       remote_addr = {ersip_host:make(RemoteAddr), RemotePort},
       transport  = SIPTransport,
       options    = Options,
       parser     =
           case IsDgram of
               false ->
                   ersip_parser:new(ParserOptions);
               true ->
                   undefined
           end
      }.

-spec conn_data(binary(), sip_conn()) -> result().
conn_data(Binary, #sip_conn{parser = undefined} = Conn) ->
    %% Datagram transport
    Parser = ersip_parser:new_dgram(Binary),
    case ersip_parser:parse(Parser) of
        {{ok, Msg}, _} ->
            receive_raw(Msg, Conn);
        {more_data, _} ->
            return_se(ersip_conn_se:bad_message(Binary, truncated), Conn);
        {{error, _} = Error, _} ->
            return_se(ersip_conn_se:bad_message(Binary, Error), Conn)
    end;
conn_data(Binary, #sip_conn{parser = Parser} = Conn) ->
    %% Stream transport
    NewParser = ersip_parser:add_binary(Binary, Parser),
    parse_data({save_parser(NewParser, Conn), []}).

-spec add_via(ersip_msg:message(), ersip_branch:branch(), sip_conn()) -> ersip_msg:message().
add_via(Msg, Branch, #sip_conn{local_addr = {LocalAddr, LocalPort}, transport = SIPTransport}) ->
    ViaH = ersip_msg:get(<<"via">>, Msg),
    Via = ersip_hdr_via:new(LocalAddr, LocalPort, SIPTransport, Branch),
    ViaH1 = ersip_hdr:add_topmost(ersip_hdr_via:assemble(Via), ViaH),
    ersip_msg:set_header(ViaH1, Msg).

-spec take_via(ersip_msg:message(), sip_conn()) -> Result when
      Result :: {ok, ersip_hdr_via:via(), ersip_msg:message()}
              | {error, no_via}
              | {error, {bad_via, term()}}
              | {error, {via_mismatch, binary(), binary()}}.
take_via(Msg, #sip_conn{} = SIPConn) ->
    ViaH = ersip_msg:get(<<"via">>, Msg),
    case ersip_hdr:take_topmost(ViaH) of
        {error, no_header} ->
            {error, no_via};
        {ok, Value, NewViaH} ->
            case ersip_hdr_via:parse(Value) of
                {ok, Via} ->
                    case check_via_match(Via, SIPConn) of
                        match ->
                            {ok, Via, ersip_msg:set_header(NewViaH, Msg)};
                        {mismatch, Expected, Got} ->
                            {error, {via_mismatch, Expected, Got}}
                    end;
                {error, Reason} ->
                    {error, {bad_via, Reason}}
            end
    end.


%%%===================================================================
%%% Internal Implementation
%%%===================================================================

-spec source(sip_conn()) -> ersip_source:source().
source(#sip_conn{remote_addr = Peer, transport = T, options = Opts}) ->
    SourceId = maps:get(source_id, Opts, undefined),
    ersip_source:new(Peer, T, SourceId).

-spec remote_ip(sip_conn()) -> ersip_host:host().
remote_ip(#sip_conn{remote_addr = {RemoteIP, _}}) ->
    RemoteIP.

-spec remote_port(sip_conn()) -> inet:port_number().
remote_port(#sip_conn{remote_addr = {_, RemotePort}}) ->
    RemotePort.

-spec save_parser(ersip_parser:data(), sip_conn()) -> sip_conn().
save_parser(Parser, SipConn) ->
    SipConn#sip_conn{parser = Parser}.

-spec receive_raw(ersip_msg:message(), sip_conn()) -> result().
receive_raw(Msg, #sip_conn{} = Conn) ->
    case ersip_msg:get(type, Msg) of
        request ->
            receive_request(Msg, Conn);
        response ->
            receive_response(Msg, Conn)
    end.

-spec receive_request(ersip_msg:message(), sip_conn()) -> result().
receive_request(Msg, Conn) ->
    case receive_request_process_via(Msg, Conn) of
        {ok, NewMsg} ->
            NewMsgWithSrc = ersip_msg:set_source(source(Conn), NewMsg),
            {Conn, [ersip_conn_se:new_request(NewMsgWithSrc)]};
        {error, _} = Error ->
            return_se(ersip_conn_se:bad_message(Msg, Error), Conn)
    end.

-spec receive_response(ersip_msg:message(), sip_conn()) -> result().
receive_response(Msg, Conn) ->
    case take_via(Msg, Conn) of
        {ok, Via, NewMsg} ->
            NewMsgWithSrc = ersip_msg:set_source(source(Conn), NewMsg),
            {Conn, [ersip_conn_se:new_response(Via, NewMsgWithSrc)]};
        {error, _} = Error ->
            {Conn, [ersip_conn_se:bad_message(Msg, Error)]}
    end.

-spec parse_data(result()) -> result().
parse_data({#sip_conn{parser= Parser} = Conn, SideEffects}) ->
    case ersip_parser:parse(Parser) of
        {more_data, NewParser} ->
            {Conn#sip_conn{parser = NewParser}, SideEffects};
        {{ok, Msg}, NewParser} ->
            Result  = receive_raw(Msg, save_parser(NewParser, Conn)),
            Result1 = add_side_effects_to_head(Result, SideEffects),
            parse_data(Result1);
        {{error, _} = Error, _} ->
            {Conn, SideEffects ++ [ersip_conn_se:disconnect(Error)]}
    end.


-spec return_se(ersip_conn_se:side_effect(), sip_conn()) -> result().
return_se(SideEffect, SipConn) ->
    {SipConn, [SideEffect]}.

-spec add_side_effects_to_head(result(), [ersip_conn_se:side_effect()]) -> result().
add_side_effects_to_head({Conn, SideEffect}, SE) ->
    {Conn, SE ++ SideEffect}.


-spec receive_request_process_via(ersip_msg:message(), sip_conn()) -> maybe_message().
receive_request_process_via(Msg, #sip_conn{} = Conn) ->
    ViaH = ersip_msg:get(<<"via">>, Msg),
    case ersip_hdr_via:topmost_via(ViaH) of
        {error, _} = Error ->
            Error;
        {ok, Via} ->
            Funs = [fun maybe_add_received/2,
                    fun maybe_fill_rport/2
                   ],
            case do_receive_request_process_via(Funs, Via, Conn) of
                {ok, Via1} ->
                    ViaH1 = ersip_hdr:replace_topmost(ersip_hdr_via:assemble(Via1), ViaH),
                    {ok, ersip_msg:set_header(ViaH1, Msg)};
                {error, _} = Error ->
                    Error
            end
    end.

-spec do_receive_request_process_via(Funs, ersip_hdr_via:via(), sip_conn()) -> maybe_via() when
      Funs :: [fun((ersip_hdr_via:via(), sip_conn()) -> maybe_via())].
do_receive_request_process_via([], Via, #sip_conn{}) ->
    {ok, Via};
do_receive_request_process_via([F | Rest], Via, #sip_conn{} = Conn) ->
    case F(Via, Conn) of
        {ok, NewVia} ->
            do_receive_request_process_via(Rest, NewVia, Conn);
        {error, _} = Error ->
            Error
    end.

%% @doc
%% When the server transport receives a request over any transport, it
%% MUST examine the value of the "sent-by" parameter in the top Via
%% header field value.  If the host portion of the "sent-by" parameter
%% contains a domain name, or if it contains an IP address that
%% differs from the packet source address, the server MUST add a
%% "received" parameter to that Via header field value.  This
%% parameter MUST contain the source address from which the packet was
%% received.
-spec maybe_add_received(ersip_msg:message(), sip_conn()) -> maybe_message().
maybe_add_received(Via, #sip_conn{} = Conn) ->
    RemoteIP = remote_ip(Conn),
    case ersip_hdr_via:sent_by(Via) of
        {sent_by, {hostname, _}, _} ->
            {ok, ersip_hdr_via:set_param(received, RemoteIP, Via)};
        {sent_by, IP, _} when IP =/= RemoteIP ->
            {ok, ersip_hdr_via:set_param(received, RemoteIP, Via)};
        _ ->
            {ok, Via}
    end.

-spec maybe_fill_rport(ersip_msg:message(), sip_conn()) -> maybe_message().
maybe_fill_rport(Via, #sip_conn{} = Conn) ->
    case ersip_hdr_via:params(Via) of
        #{rport := true} ->
            %% RFC 3581:
            %% When a server compliant to this specification (which can be a proxy
            %% or UAS) receives a request, it examines the topmost Via header field
            %% value.  If this Via header field value contains an "rport" parameter
            %% with no value, it MUST set the value of the parameter to the source
            %% port of the request.  This is analogous to the way in which a server
            %% will insert the "received" parameter into the topmost Via header
            %% field value.  In fact, the server MUST insert a "received" parameter
            %% containing the source IP address that the request came from, even if
            %% it is identical to the value of the "sent-by" component.  Note that
            %% this processing takes place independent of the transport protocol.
            Via1 = ersip_hdr_via:set_param(rport, remote_port(Conn), Via),
            Via2 = ersip_hdr_via:set_param(received, remote_ip(Conn), Via1),
            {ok, Via2};
        _ ->
            {ok, Via}
    end.

-spec check_via_match(ersip_hdr_via:via(), sip_conn()) -> Result when
      Result :: match
              | {mismatch, Expected :: binary(), Got :: binary()}.
check_via_match(Via, #sip_conn{local_addr = {LocalAddr, LocalPort}, transport = SIPTransport} = SipConn) ->
    Match = check_via_match_address(Via, SipConn)
        andalso check_via_match_transport(Via, SipConn),
    case Match of
        true ->
            match;
        false ->
            Expected = ersip_hdr_via:new(LocalAddr, LocalPort, SIPTransport),
            ExpectedBin = iolist_to_binary(ersip_hdr_via:assemble(Expected)),
            GotBin = iolist_to_binary(ersip_hdr_via:assemble(Via)),
            {mismatch, ExpectedBin, GotBin}
    end.

-spec check_via_match_address(ersip_hdr_via:via(), sip_conn()) -> boolean().
check_via_match_address(Via, #sip_conn{local_addr = {LocalAddr, LocalPort}}) ->
    case ersip_hdr_via:sent_by(Via) of
        {sent_by, LocalAddr, LocalPort} ->
            true;
        _ ->
            false
    end.

-spec check_via_match_transport(ersip_hdr_via:via(), sip_conn()) -> boolean().
check_via_match_transport(Via, #sip_conn{transport = SIPTransport}) ->
    case ersip_hdr_via:sent_protocol(Via) of
        {sent_protocol, <<"SIP">>, <<"2.0">>, SIPTransport} ->
            true;
        _ ->
            false
    end.
