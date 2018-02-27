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

-export([ new/4,
          conn_data/2
        ]).
-export_type([ sip_conn/0 ]).

%%%===================================================================
%%% Types
%%%===================================================================

-type result() :: { sip_conn(), [ ersip_conn_se:effect() ] }.
-record(sip_conn, {
          local_addr :: { inet:ip_address(), inet:port_number() },
          transport  :: ersip_transport:transport(),
          options    :: options(),
          parser     :: ersip_parser:data() | undefined,
          is_dgram   :: boolean()
         }).
-type sip_conn() :: #sip_conn{}.
-type options()  :: map().

%%%===================================================================
%%% API
%%%===================================================================

-spec new(LocalAddr, LocalPort, SIPTransport, Options) -> sip_conn() when
      LocalAddr    :: inet:ip_address(),
      LocalPort    :: inet:port_number(),
      SIPTransport :: ersip_transport:transport(),
      Options      :: options().
new(LocalAddr, LocalPort, SIPTransport, Options) ->
    ParserOptions = maps:get(parser, Options, #{}),
    IsDgram       = ersip_transport:is_dgram(SIPTransport),
    #sip_conn{
       local_addr = { LocalAddr, LocalPort },
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
conn_data(Binary, #sip_conn{ parser = undefined } = Conn) -> 
    %% Datagram transport
    Parser = ersip_parser:new_dgram(Binary),
    case ersip_parser:parse(Parser) of
        { ok, Msg } ->
            receive_raw(Msg, Conn);
        { error, _ } = Error ->
            return_se(ersip_conn_se:bad_datagram(Binary, Error), Conn)
    end;
conn_data(Binary, #sip_conn{ parser = Parser } = Conn) -> 
    %% Stream transport
    NewParser = ersip_parser:add_binary(Binary, Parser),
    parse_data({ save_parser(NewParser, Conn), [] }).
                
%%%===================================================================
%%% Internal Implementation
%%%===================================================================

-spec save_parser(ersip_parser:data(), sip_conn()) -> sip_conn().
save_parser(Parser, SipConn) ->
    SipConn#sip_conn{ parser = Parser }.

-spec receive_raw(ersip_msg:message(), sip_conn()) -> result().
receive_raw(Msg, Conn) ->
    { Conn, [] }.

-spec parse_data(result()) -> result().
parse_data({ #sip_conn{ parser= Parser } = Conn, SideEffects }) ->
    case ersip_parser:parse(Parser) of
        { more_data, NewParser } ->
            { Conn#sip_conn{ parser = NewParser }, SideEffects };
        { {ok, Msg }, NewParser } ->
            Result  = receive_raw(Msg, save_parser(NewParser, Conn)),
            Result1 = add_side_effects_to_head(Result, SideEffects),
            parse_data(Result1);
        { error, _ } = Error ->
            { Conn, SideEffects ++ [ ersip_conn_se:disconnect(Error) ] }
    end.


-spec return_se(ersip_conn_se:side_effect(), sip_conn()) -> result().
return_se(SideEffect, SipConn) ->
    { SipConn, [ SideEffect ] }.

add_side_effects_to_head({ Conn, SideEffect }, SE) ->
    { Conn, SE ++ SideEffect }.

