%%
%% Copyright (c) 2017 Dmitry Poroh
%% All rights reserved.
%% Distributed under the terms of the MIT License. See the LICENSE file.
%%
%% Erlang SIP
%%

-module(ersip).

-type transport()      :: {transport, transport_atom()}.
-type message()        :: ersip_msg:message().
-type uri()            :: ersip_uri:uri().
-type nexthop()        :: {peer, ersip_host:host(), Port :: 0..65536, transport()}
                        | {conn, connection_id()}.

-export_type([transport/0,
              message/0,
              uri/0,
              nexthop/0
             ]).

-type transport_atom() :: udp | tcp | tls | ws | wss | sctp.
-type connection_id()  :: term().

-type sip_options() :: #{sip_t1 => pos_integer(),
                         sip_t2 => pos_integer(),
                         sip_t4 => pos_integer(),
                         %% This is custom extension of ersip that
                         %% limits transaction time (Timer B/Timer F),
                         %% if set then timeout is indicated after
                         %% this time and not after 64*T1
                         trans_expire => pos_integer()
                        }.
-export_type([sip_options/0]).
