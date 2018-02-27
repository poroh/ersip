%%
%% Copyright (c) 2018 Dmitry Poroh
%% All rights reserved.
%% Distributed under the terms of the MIT License. See the LICENSE file.
%%
%% SIP Port
%%
%% Describes SIP transport entity that listens
%% One IP address port pair
%%

-module(ersip_port).

-export([ new/4,
          port_data/2
        ]).
-export_type([ sip_port/0 ]).

%%%===================================================================
%%% Types
%%%===================================================================

-type result() :: { sip_port(), [ ersip_port_se:effect() ] }.
-record(sip_port, {
          local_addr :: { inet:ip_address(), inet:port_number() },
          transport  :: ersip_transport:transport(),
          options    :: options(),
          parser     :: ersip_parser:data() | undefined,
          is_dgram   :: boolean()
         }).
-type sip_port() :: #sip_port{}.
-type options()  :: map().

%%%===================================================================
%%% API
%%%===================================================================

-spec new(LocalAddr, LocalPort, SIPTransport, Options) -> sip_port() when
      LocalAddr    :: inet:ip_address(),
      LocalPort    :: inet:port_number(),
      SIPTransport :: ersip_transport:transport(),
      Options      :: options().
new(LocalAddr, LocalPort, SIPTransport, Options) ->
    ParserOptions = maps:get(parser, Options, #{}),
    IsDgram       = ersip_transport:is_dgram(SIPTransport),
    #sip_port{
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

-spec port_data(binary(), sip_port()) -> result().
port_data(Binary, #sip_port{ parser = undefined } = Port) -> 
    %% Datagram transport
    Buf = ersip_buf:new_dgram(Binary),
    case ersip_parser:parse(Buf) of
        { ok, Msg } ->
            receive_raw(Msg, Port);
        { error, _ } = Error ->
            return_se(ersip_port_se:bad_datagram(Binary, Error), Port)
    end;
port_data(Binary, #sip_port{ parser = Parser } = Port) -> 
    %% Stream transport
    case ersip_parser:add_binary(Binary, Parser) of
        { more_data, NewParser } ->
            { Port#sip_port{ parser = NewParser }, [] };
        { {ok, Msg }, NewParser } ->
            Result = receive_raw(Msg, save_parser(NewParser, Port)),
            maybe_receive_more(Result);
        { error, _ } = Error ->
            return_se(disconnect, Port)
    end.
                
%%%===================================================================
%%% Internal Implementation
%%%===================================================================

-spec save_parser(ersip_parser:data(), sip_port()) -> sip_port().
save_parser(Parser, SipPort) ->
    SipPort#sip_port{ parser = Parser }.

-spec receive_raw(ersip_msg:message(), sip_port()) -> result().
receive_raw(Msg, Port) ->
    { Port, [] }.

-spec return_se(ersip_port_se:side_effect(), sip_port()) -> result().
return_se(SideEffect, SipPort) ->
    { SipPort, [ SideEffect ] }.


maybe_receive_more(X) ->
    X.
