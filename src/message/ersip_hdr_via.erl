%%
%% Copyright (c) 2018 Dmitry Poroh
%% All rights reserved.
%% Distributed under the terms of the MIT License. See the LICENSE file.
%%
%% SIP Via header
%%

-module(ersip_hdr_via).

-export([new/3,
         new/4,
         topmost_via/1,
         sent_protocol/1,
         branch/1,
         set_branch/2,
         received/1,
         set_received/2,
         has_rport/1,
         rport/1,
         set_rport/2,
         maddr/1,
         set_maddr/2,
         ttl/1,
         set_ttl/2,
         raw_param/2,
         set_param/3,
         sent_by/1,
         sent_by_key/1,
         make_key/1,
         assemble/1,
         assemble_bin/1,
         parse/1
        ]).

-export_type([via/0,
              via_key/0,
              sent_by/0
             ]).

%%%===================================================================
%%% Types
%%%===================================================================

-record(via, {sent_protocol  :: sent_protocol(),
              sent_by        :: internal_sent_by(),
              hparams        :: ersip_hparams:hparams()
             }).
-type via()           :: #via{}.
-type sent_protocol() :: {sent_protocol, Protocol :: binary(), ProtocolVersion :: binary(), ersip_transport:transport()}.
-type sent_by()       :: {sent_by, ersip_host:host(), Port :: ersip_transport:port_number()}.
-type internal_sent_by() :: {sent_by, ersip_host:host(), Port :: ersip_transport:port_number() | default_port}.
-type known_via_params() :: branch
                          | maddr
                          | received
                          | ttl
                          | rport.
-type rport_value() :: ersip_transport:port_number() | true.
-type ttl_value()   :: 0..255.
-type via_key() :: {sent_protocol(), sent_by(), via_params_key()}.
-type via_params_key()    :: #{branch   => ersip_branch:branch(),
                               maddr    => ersip_host:host(),
                               received => ersip_host:host(),
                               ttl      => non_neg_integer(),
                               rport    => ersip_transport:port_number() | true,
                               binary() => binary()
                              }.

%%%===================================================================
%%% API
%%%===================================================================

-spec new(ersip_host:host(), ersip_transport:port_number(), ersip_transport:transport()) -> via().
new(Address, Port, Transport) ->
    #via{sent_protocol = {sent_protocol, <<"SIP">>, <<"2.0">>, Transport},
         sent_by = {sent_by, Address, Port},
         hparams = ersip_hparams:new()
        }.

-spec new(ersip_host:host(), ersip_transport:port_number(), ersip_transport:transport(), ersip_branch:branch()) -> via().
new(Address, Port, Transport, Branch) ->
    HParams0 = ersip_hparams:new(),
    HParams = ersip_hparams:set(branch, Branch, <<"branch">>, ersip_branch:assemble(Branch), HParams0),
    #via{sent_protocol = {sent_protocol, <<"SIP">>, <<"2.0">>, Transport},
         sent_by = {sent_by, Address, Port},
         hparams = HParams
        }.

-spec topmost_via(ersip_hdr:header()) -> Result when
      Result :: {ok, via()}
              | {error, Error},
      Error  :: no_via.
topmost_via(Header) ->
    case ersip_hdr:raw_values(Header) of
        [] ->
            {error, no_via};
        [TopVia | _]  ->
            parse_via(iolist_to_binary(TopVia))
    end.

-spec sent_protocol(via()) -> sent_protocol().
sent_protocol(#via{sent_protocol = Sp}) ->
    Sp.

-spec raw_param(ParamName :: binary(), via()) -> {ok, ParamValue :: binary()} | not_found.
raw_param(ParamName, #via{hparams = HParams}) when is_binary(ParamName) ->
    ersip_hparams:find_raw(ParamName, HParams).

-spec set_param(ParamName, Value, via()) -> via() when
      ParamName :: known_via_params() | binary(),
      Value     :: binary()
                 | ersip_host:host()
                 | ersip_transport:port_number()
                 | ersip_branch:branch().
set_param(received, Value, Via) when is_binary(Value) ->
    case ersip_host:parse(Value) of
        {ok, Host, <<>>} ->
            set_param(received, Host, Via);
        {ok, _, _} ->
            error({error, {invalid_host, Value}});
        {error, _} = Error ->
            error(Error)
    end;
set_param(received, {Type, _} = Value , #via{hparams = HP} = Via)
  when Type =:= ipv4 orelse Type =:= ipv6 ->
    HPNew = ersip_hparams:set(received, Value, <<"received">>, assemble_param_value(received, Value), HP),
    Via#via{hparams = HPNew};
set_param(received, Value, _) ->
    error({error, {bad_received_via_param, Value}});
set_param(rport, Value, #via{hparams = HP} = Via)
  when is_integer(Value) andalso Value >= 1 andalso Value =< 65535 orelse Value == true ->
    HPNew = ersip_hparams:set(rport, Value, <<"rport">>, assemble_param_value(rport, Value), HP),
    Via#via{hparams = HPNew};
set_param(maddr, Host , #via{hparams = HP} = Via) ->
    HPNew = ersip_hparams:set(maddr, Host, <<"maddr">>, assemble_param_value(maddr, Host), HP),
    Via#via{hparams = HPNew};
set_param(ttl, Value, #via{hparams = HP} = Via)
  when is_integer(Value) andalso Value >= 0 andalso Value =< 255 ->
    HPNew = ersip_hparams:set(ttl, Value, <<"ttl">>, assemble_param_value(ttl, Value), HP),
    Via#via{hparams = HPNew};
set_param(branch, {branch, _} = Value, #via{hparams = HP} = Via) ->
    HPNew = ersip_hparams:set(branch, Value, <<"branch">>, assemble_param_value(branch, Value), HP),
    Via#via{hparams = HPNew};
set_param(rport, Value, _) ->
    error({error, {bad_rport_via_param, Value}}).

-spec sent_by(via()) -> sent_by().
sent_by(#via{sent_by = {sent_by,Host,default_port}} = Via) ->
    {sent_protocol, _, _, Transport} = sent_protocol(Via),
    {sent_by, Host, ersip_transport:default_port(Transport)};
sent_by(#via{sent_by = SentBy}) ->
    SentBy.

%% @doc Make comparable sent_by (adjusted to be comparable as erlang
%% terms).
-spec sent_by_key(via()) -> sent_by().
sent_by_key(#via{} = Via) ->
    sent_by_make_key(sent_by(Via)).

-spec branch(via()) -> {ok, ersip_branch:branch()} | undefined.
branch(#via{hparams = HParams}) ->
    case ersip_hparams:find(branch, HParams) of
        {ok, BranchValue} -> {ok, BranchValue};
        not_found -> undefined
    end.

-spec set_branch(ersip_branch:branch(), via()) -> via().
set_branch(Branch, #via{} = Via) ->
    set_param(branch, Branch, Via).

-spec received(via()) -> {ok, ersip_host:host()} | undefined.
received(#via{hparams = HParams}) ->
    case ersip_hparams:find(received, HParams) of
        {ok, Host} -> {ok, Host};
        not_found  -> undefined
    end.

-spec set_received(ersip_host:host(), via()) -> via().
set_received(Host, #via{} = Via) ->
    set_param(received, Host, Via).

-spec has_rport(via()) -> boolean().
has_rport(#via{hparams = HParams}) ->
    case ersip_hparams:find(rport, HParams) of
        {ok, _} ->   true;
        not_found -> false
    end.

-spec rport(via()) -> {ok, rport_value()} | undefined.
rport(#via{hparams = HParams}) ->
    case ersip_hparams:find(rport, HParams) of
        {ok, RPortVal} -> {ok, RPortVal};
        not_found      -> undefined
    end.

-spec set_rport(rport_value(), via()) -> via().
set_rport(RPort, #via{} = Via) ->
    set_param(rport, RPort, Via).

-spec maddr(via()) -> {ok, ersip_host:host()} | undefined.
maddr(#via{hparams = HParams}) ->
    case ersip_hparams:find(maddr, HParams) of
        {ok, Host} -> {ok, Host};
        not_found  -> undefined
    end.

-spec set_maddr(ersip_host:host(), via()) -> via().
set_maddr(Host, #via{} = Via) ->
    set_param(maddr, Host, Via).

-spec ttl(via()) -> {ok, ttl_value()} | undefined.
ttl(#via{hparams = HParams}) ->
    case ersip_hparams:find(ttl, HParams) of
        {ok, TTL} -> {ok, TTL};
        not_found -> undefined
    end.

-spec set_ttl(ttl_value(), via()) -> via().
set_ttl(TTLValue, #via{} = Via) ->
    set_param(ttl, TTLValue, Via).

-spec make_key(via()) -> via_key().
make_key(#via{hparams = HParams} = Via) ->
    {sent_protocol_make_key(sent_protocol(Via)),
     sent_by_make_key(sent_by(Via)),
     via_params_make_key(HParams)}.

-spec assemble(via()) -> iolist().
assemble(#via{} = Via) ->
    #via{
       sent_protocol =
           {sent_protocol, Protocol, ProtocolVersion, Transport},
       sent_by    = {sent_by, Host, Port},
       hparams = HParams
      } = Via,
    [Protocol, $/, ProtocolVersion, $/, ersip_transport:assemble_upper(Transport),
     <<" ">>, ersip_host:assemble(Host),
     case Port of
         default_port ->
             [];
         Port ->
             [$:, integer_to_binary(Port)]
     end,
     assemble_params(HParams)
    ].

-spec assemble_bin(via()) -> binary().
assemble_bin(#via{} = Via) ->
    iolist_to_binary(assemble(Via)).

-spec parse(iolist()) -> {ok, via()} | {error, term()}.
parse(IOList) ->
    parse_via(iolist_to_binary(IOList)).

%%%===================================================================
%%% Internal implementation
%%%===================================================================

-spec parse_via(binary()) -> {ok,via()} | {error, term()}.
parse_via(ViaBinary) ->
    Parsers = [fun parse_sent_protocol/1,
               fun ersip_parser_aux:parse_lws/1,
               fun parse_sent_by/1,
               fun ersip_parser_aux:trim_lws/1,
               fun parse_via_params/1],
    case ersip_parser_aux:parse_all(ViaBinary, Parsers) of
        {ok, [SentProtocol, _, SentBy, _, HParams], _} ->
            Via = #via{sent_protocol = SentProtocol,
                       sent_by       = SentBy,
                       hparams       = HParams},
            {ok, Via};
        {error, _} = Error ->
            Error
    end.

%% sent-protocol     =  protocol-name SLASH protocol-version
%%                      SLASH transport
%% protocol-name     =  "SIP" / token
%% protocol-version  =  token
%% transport         =  "UDP" / "TCP" / "TLS" / "SCTP"
%%                      / other-transport
-spec parse_sent_protocol(binary()) -> ersip_parser_aux:parse_result().
parse_sent_protocol(Binary) ->
    Parsers = [fun ersip_parser_aux:parse_token/1,
               fun ersip_parser_aux:parse_slash/1,
               fun ersip_parser_aux:parse_token/1,
               fun ersip_parser_aux:parse_slash/1,
               fun parse_transport/1
              ],
    case ersip_parser_aux:parse_all(Binary, Parsers) of
        {ok, [ProtocolName, _, ProtocolVersion, _, Transport], Rest} ->
            {ok, {sent_protocol, ProtocolName, ProtocolVersion, Transport}, Rest};
        {error, _} = Error ->
            Error
    end.

%% sent-by           =  host [COLON port]
%% this implementation is not pure. It uses sent-by context in via
%% header (expects that either EOL or SEMI occur after sent-by).
-spec parse_sent_by(binary()) -> ersip_parser_aux:parse_result().
parse_sent_by(Binary) ->
    case binary:match(Binary, <<";">>) of
        {Pos, 1} ->
            HostPort = binary:part(Binary, {0, Pos}),
            Rest = binary:part(Binary, Pos, byte_size(Binary) - Pos),
            parse_sent_by_host_port(ersip_bin:trim_lws(HostPort), host, #{rest => Rest});
        nomatch ->
            parse_sent_by_host_port(ersip_bin:trim_lws(Binary), host, #{rest => <<>>})
    end.

parse_sent_by_host_port(<<$[, _/binary>> = IPv6RefPort, host, Acc) ->
    case binary:match(IPv6RefPort, <<"]">>) of
        nomatch ->
            {error, {invalid_ipv6_reference, IPv6RefPort}};
        {Pos, 1} ->
            HostBin = binary:part(IPv6RefPort, {0, Pos+1}),
            Rest = binary:part(IPv6RefPort, {Pos+1, byte_size(IPv6RefPort)-Pos-1}),
            case ersip_host:parse(HostBin) of
                {ok, Host, <<>>} ->
                    parse_sent_by_host_port(Rest, port, Acc#{host => Host});
                {ok, _Host, _Rest} ->
                    {error, {invalid_ipv6_reference, IPv6RefPort}};
                {error, _} = Err ->
                    Err
            end
    end;
parse_sent_by_host_port(Binary, host, Acc) ->
    [HostBin | MayBePort] = binary:split(Binary, <<":">>),
    case ersip_host:parse(HostBin) of
        {ok, Host, <<>>} ->
            parse_sent_by_host_port(MayBePort, port, Acc#{host => Host});
        {ok, _Host, _Rest} ->
            {error, {invalid_host, HostBin}};
        {error, _} = Err ->
            Err
    end;
parse_sent_by_host_port([], port, Acc) ->
    parse_sent_by_host_port(<<>>, result, Acc);
parse_sent_by_host_port(<<>>, port, Acc) ->
    parse_sent_by_host_port(<<>>, result, Acc);
parse_sent_by_host_port(<<":", Rest/binary>>, port, Acc) ->
    parse_sent_by_host_port([Rest], port, Acc);
parse_sent_by_host_port([Bin], port, Acc) when is_binary(Bin) ->
    case ersip_transport:parse_port_number(ersip_bin:trim_lws(Bin)) of
        {ok, PortNumber, <<>>} ->
            parse_sent_by_host_port(<<>>, result, Acc#{port => PortNumber});
        _ ->
            {error, {invalid_port, Bin}}
    end;
parse_sent_by_host_port(_, result, #{rest := Rest, host := Host} = Acc) ->
    Port = maps:get(port, Acc, default_port),
    {ok, {sent_by, Host, Port}, Rest}.

-spec parse_transport(binary()) -> ersip_parser_aux:parse_result(ersip_transport:transport()).
parse_transport(Binary) ->
    case ersip_parser_aux:parse_token(Binary) of
        {ok, Transp, Rest} ->
            {ok, T} = ersip_transport:parse(Transp),
            {ok, T, Rest};
        {error, _} = Error ->
            Error
    end.

-spec parse_via_params(binary()) -> ersip_parser_aux:parse_result(ersip_hparams:hparams()).
parse_via_params(<<$;, Rest/binary>>) ->
    do_parse_via_params(Rest);
parse_via_params(Binary) ->
    {ok, ersip_hparams:new(), Binary}.

-spec do_parse_via_params(binary()) -> ersip_parser_aux:parse_result(ersip_hparams:hparams()).
do_parse_via_params(Bin) ->
    case ersip_hparams:parse_raw(Bin) of
        {ok, HParams0, Rest} ->
            case ersip_hparams:parse_known(fun parse_known/2, HParams0) of
                {ok, HParams} ->
                    {ok, HParams, Rest};
                {error, _} = Error ->
                    Error
            end;
        {error, _} = Error ->
            Error
    end.

%% via-params        =  via-ttl / via-maddr
%%                      / via-received / via-branch
%%                      / via-extension
-spec parse_known(Key, Value) -> Result when
      Key    :: binary(),
      Value  :: binary(),
      Result :: {ok, {ResultKey, ResultVal}}
              | {error, term()},
      ResultKey :: known_via_params() | binary(),
      ResultVal :: non_neg_integer()
                 | ersip_host:host()
                 | binary().
parse_known(<<"ttl">>, Value) ->
    %% 1*3DIGIT ; 0 to 255
    case ersip_parser_aux:parse_non_neg_int(Value) of
        {ok, TTL, <<>>} when TTL >= 0 andalso TTL =< 255 ->
            {ok, {ttl, TTL}};
        _ ->
            {error, {invalid_ttl, Value}}
    end;
parse_known(<<"received">>, Value) ->
    %% "received" EQUAL (IPv4address / IPv6address)
    case ersip_host:parse(Value) of
        {ok, {ipv4, _} = Host, <<>>} ->
            {ok, {received, Host}};
        {ok, {ipv6, _} = Host, <<>>} ->
            {ok, {received, Host}};
        _ ->
            {error, {invalid_received, Value}}
    end;
parse_known(<<"maddr">>, Value) ->
    %% "received" EQUAL (IPv4address / IPv6address)
    case ersip_host:parse(Value) of
        {ok, Host, <<>>} ->
            {ok, {maddr, Host}};
        _ ->
            {error, {invalid_maddr, Value}}
    end;
parse_known(<<"branch">>, Value) ->
    %% "branch" EQUAL token
    case ersip_parser_aux:parse_token(Value) of
        {ok, Branch, <<>>} ->
            {ok, {branch, ersip_branch:make(Branch)}};
        _ ->
            {error, {invalid_branch, Value}}
    end;
parse_known(<<"rport">>, <<>>) ->
    {ok, {rport, true}};
parse_known(<<"rport">>, Value) ->
    %% response-port = "rport" [EQUAL 1*DIGIT]
    case ersip_transport:parse_port_number(Value) of
        {ok, Port, <<>>} ->
            {ok, {rport, Port}};
        _ ->
            {error, {invalid_rport, Value}}
    end;
parse_known(_, _) ->
    {ok, unknown}.

-spec sent_protocol_make_key(sent_protocol()) -> sent_protocol().
sent_protocol_make_key(Protocol) ->
    Protocol.

-spec sent_by_make_key(sent_by()) -> sent_by().
sent_by_make_key({sent_by, Host, Port}) ->
    {sent_by, ersip_host:make_key(Host), Port}.

-spec via_params_make_key(ersip_hparams:hparams()) -> via_params_key().
via_params_make_key(Params) ->
    L = ersip_hparams:to_list(Params),
    LKeys = lists:map(fun({Key, Value}) ->
                              via_param_make_key(Key, Value)
                      end,
                      L),
    maps:from_list(LKeys).

-spec via_param_make_key(Key, Value) -> {NewKey, NewValue} when
      Key    :: known_via_params() | binary(),
      Value  :: term(),
      NewKey :: known_via_params() | binary(),
      NewValue :: term().
via_param_make_key(ttl, V) ->
    {ttl, V};
via_param_make_key(branch, B) ->
    {branch, ersip_branch:make_key(B)};
via_param_make_key(maddr, Maddr) ->
    {maddr, ersip_host:make_key(Maddr)};
via_param_make_key(received, R) ->
    {received, R};
via_param_make_key(rport, R) ->
    {rport, R};
via_param_make_key(OtherKey, OtherValue) when is_binary(OtherKey) ->
    {ersip_bin:to_lower(OtherKey), OtherValue}.

-spec assemble_params(ersip_hparams:hparams()) -> [iolist()].
assemble_params(HParams) ->
    HParamsIO0 = ersip_hparams:assemble(HParams),
    case ersip_iolist:is_empty(HParamsIO0) of
        true -> [];
        false -> [$; | HParamsIO0]
    end.

-spec assemble_param_value(known_via_params(), term()) -> binary().
assemble_param_value(received, Value) ->
    iolist_to_binary(ersip_host:assemble(Value));
assemble_param_value(rport, true) ->
    <<>>;
assemble_param_value(rport, Value) ->
    integer_to_binary(Value);
assemble_param_value(ttl, Value) ->
    integer_to_binary(Value);
assemble_param_value(maddr, Value) ->
    iolist_to_binary(ersip_host:assemble(Value));
assemble_param_value(branch, Value) ->
    ersip_branch:assemble(Value).
