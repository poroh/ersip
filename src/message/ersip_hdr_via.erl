%%%
%%% Copyright (c) 2018, 2020 Dmitry Poroh
%%% All rights reserved.
%%% Distributed under the terms of the MIT License. See the LICENSE file.
%%%
%%% SIP Via header
%%%

-module(ersip_hdr_via).

-export([new/3,
         new/4,
         topmost_via/1,
         take_topmost/1,
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
         all_raw_params/1,
         set_param/3,
         sent_by/1,
         sent_by_key/1,
         make_key/1,
         assemble/1,
         assemble_bin/1,
         make/1,
         parse/1,
         raw/1
        ]).

-export_type([via/0,
              via_key/0,
              sent_by/0,
              raw/0
             ]).

%%===================================================================
%% Types
%%===================================================================

-record(via, {sent_protocol  :: sent_protocol(),
              sent_by        :: internal_sent_by(),
              hparams        :: ersip_hparams:hparams()
             }).
-type via()           :: #via{}.
-type sent_protocol() :: {sent_protocol, Protocol :: binary(), ProtocolVersion :: binary(), ersip_transport:transport()}.
-type sent_by()       :: {sent_by, ersip_host:host(), Port :: inet:port_number()}.
-type internal_sent_by() :: {sent_by, ersip_host:host(), Port :: inet:port_number() | default_port}.
-type known_via_params() :: branch
                          | maddr
                          | received
                          | ttl
                          | rport.
-type rport_value() :: inet:port_number() | true.
-type ttl_value()   :: 0..255.
-type via_key() :: {sent_protocol(), sent_by(), via_params_key()}.
-type via_params_key()    :: #{branch   => ersip_branch:branch(),
                               maddr    => ersip_host:host(),
                               received => ersip_host:host(),
                               ttl      => non_neg_integer(),
                               rport    => inet:port_number() | true,
                               binary() => binary()
                              }.
-type raw() :: #{protocol      := binary(),
                 version       := binary(),
                 transport     := binary(),
                 host          := binary(),
                 port          := inet:port_number(),
                 params        := ersip_hparams:raw(),
                 branch        => binary(),
                 maddr         => binary(),
                 received      => binary(),
                 ttl           => non_neg_integer(),
                 rport         => rport_value()
                }.
%%===================================================================
%% API
%%===================================================================

%% @doc Create new Via haeder by Host, Port and SIP transport.
-spec new(ersip_host:host(), inet:port_number(), ersip_transport:transport()) -> via().
new(Address, Port, Transport) ->
    #via{sent_protocol = {sent_protocol, <<"SIP">>, <<"2.0">>, Transport},
         sent_by = {sent_by, Address, Port},
         hparams = ersip_hparams:new()
        }.

%% @doc Create new Via haeder by Host, Port, SIP transport and branch parameter.
-spec new(ersip_host:host(), inet:port_number(), ersip_transport:transport(), ersip_branch:branch()) -> via().
new(Address, Port, Transport, Branch) ->
    HParams0 = ersip_hparams:new(),
    HParams = ersip_hparams:set(branch, Branch, <<"branch">>, ersip_branch:assemble(Branch), HParams0),
    #via{sent_protocol = {sent_protocol, <<"SIP">>, <<"2.0">>, Transport},
         sent_by = {sent_by, Address, Port},
         hparams = HParams
        }.

%% @doc Get topmost Via from SIP raw headers.
-spec topmost_via(ersip_hdr:header()) ->  {ok, via()} | {error, no_via}.
topmost_via(Header) ->
    case ersip_hdr:raw_values(Header) of
        [] -> {error, no_via};
        [TopVia | _]  ->
            case parse_via_with_rest(iolist_to_binary(TopVia)) of
                {error, _} = Error -> Error;
                {ok, Via, Rest} ->
                    case validate_topmost_via(Via, Rest) of
                        ok -> {ok, Via};
                        {error, Reason} ->
                            {error, {invalid_via, Reason}}
                    end
            end
    end.

-spec take_topmost(iolist()) -> ersip_parser_aux:parse_result(via()).
take_topmost(IOList) ->
    Bin = iolist_to_binary(IOList),
    case parse_via_with_rest(Bin) of
        {ok, Via, <<",", Rest/binary>>} ->
            case validate_topmost_via(Via) of
                ok -> {ok, Via, Rest};
                {error, Reason} -> {error, {invalid_via, Reason}}
            end;
        {ok, Via, <<>>} ->
            case validate_topmost_via(Via) of
                ok -> {ok, Via, <<>>};
                {error, Reason} -> {error, {invalid_via, Reason}}
            end;
        {ok, _, Rest} ->
            {error, {invalid_via, {invalid_via, {garbage_at_the_end, Rest}}}};
        {error, Reason} ->
            {error, {invalid_via, Reason}}
    end.

%% @doc Get sent protocol.
-spec sent_protocol(via()) -> sent_protocol().
sent_protocol(#via{sent_protocol = Sp}) ->
    Sp.

%% @doc Get Via parametr by it's name.
-spec raw_param(ParamName :: binary(), via()) -> {ok, ParamValue :: binary()} | not_found.
raw_param(ParamName, #via{hparams = HParams}) when is_binary(ParamName) ->
    ersip_hparams:find_raw(ParamName, HParams).

%% @doc Get all Via parameters.
-spec all_raw_params(via()) -> [{binary(), binary()} | binary()].
all_raw_params(#via{hparams = HParams}) ->
    ersip_hparams:to_raw_list(HParams).

%% @doc Get Via parameter.  This function supports known paramters
%% (deprecated) and binary parameters.
-spec set_param(known_via_params() | binary(), any(), via()) -> via().
set_param(received, Value, Via) when is_binary(Value) ->
    case ersip_host:parse(Value) of
        {ok, Host, <<>>} -> set_param(received, Host, Via);
        {ok, _, _}       -> error({invalid_received, Value});
        {error, Reason}  -> error({invalid_received, Reason})
    end;
set_param(received, {Type, _} = Value , #via{hparams = HP} = Via)
  when Type =:= ipv4 orelse Type =:= ipv6 ->
    HPNew = ersip_hparams:set(received, Value, <<"received">>, assemble_received(Value), HP),
    Via#via{hparams = HPNew};
set_param(received, Value, _) ->
    error({invalid_received, Value});
set_param(rport, Value, #via{hparams = HP} = Via)
  when is_integer(Value) andalso Value >= 1 andalso Value =< 65535 orelse Value == true ->
    HPNew = ersip_hparams:set(rport, Value, <<"rport">>, assemble_rport(Value), HP),
    Via#via{hparams = HPNew};
set_param(maddr, Host , #via{hparams = HP} = Via) ->
    HPNew = ersip_hparams:set(maddr, Host, <<"maddr">>, assemble_maddr(Host), HP),
    Via#via{hparams = HPNew};
set_param(ttl, Value, #via{hparams = HP} = Via)
  when is_integer(Value) andalso Value >= 0 andalso Value =< 255 ->
    HPNew = ersip_hparams:set(ttl, Value, <<"ttl">>, assemble_ttl(Value), HP),
    Via#via{hparams = HPNew};
set_param(branch, {branch, _} = Value, #via{hparams = HP} = Via) ->
    HPNew = ersip_hparams:set(branch, Value, <<"branch">>, ersip_branch:assemble_bin(Value), HP),
    Via#via{hparams = HPNew};
set_param(rport, Value, _) ->
    error({invalid_rport, Value}).

%% @doc Get Host and port from Via header.
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

%% @doc Get branch parameter.
-spec branch(via()) -> {ok, ersip_branch:branch()} | undefined.
branch(#via{hparams = HParams}) ->
    case ersip_hparams:find(branch, HParams) of
        {ok, BranchValue} -> {ok, BranchValue};
        not_found -> undefined
    end.

%% @doc Set branch parameter.
-spec set_branch(ersip_branch:branch(), via()) -> via().
set_branch(Branch, #via{} = Via) ->
    set_param(branch, Branch, Via).

%% @doc Get received parameter.
-spec received(via()) -> {ok, ersip_host:host()} | undefined.
received(#via{hparams = HParams}) ->
    case ersip_hparams:find(received, HParams) of
        {ok, Host} -> {ok, Host};
        not_found  -> undefined
    end.

%% @doc Set received parameter.
-spec set_received(ersip_host:host(), via()) -> via().
set_received(Host, #via{} = Via) ->
    set_param(received, Host, Via).

%% @doc Check if Via has rport parameter.
-spec has_rport(via()) -> boolean().
has_rport(#via{hparams = HParams}) ->
    case ersip_hparams:find(rport, HParams) of
        {ok, _} ->   true;
        not_found -> false
    end.

%% @doc Get rport parameter value.  If rport parameter is set without
%% port number than functio returns {ok, true}.
-spec rport(via()) -> {ok, rport_value()} | undefined.
rport(#via{hparams = HParams}) ->
    case ersip_hparams:find(rport, HParams) of
        {ok, RPortVal} -> {ok, RPortVal};
        not_found      -> undefined
    end.

%% @doc Set rport parameter value.
-spec set_rport(rport_value(), via()) -> via().
set_rport(RPort, #via{} = Via) ->
    set_param(rport, RPort, Via).

%% @doc Get maddr parameter.
-spec maddr(via()) -> {ok, ersip_host:host()} | undefined.
maddr(#via{hparams = HParams}) ->
    case ersip_hparams:find(maddr, HParams) of
        {ok, Host} -> {ok, Host};
        not_found  -> undefined
    end.

%% @doc Set maddr parameter.
-spec set_maddr(ersip_host:host(), via()) -> via().
set_maddr(Host, #via{} = Via) ->
    set_param(maddr, Host, Via).

%% @doc Get ttl parameter.
-spec ttl(via()) -> {ok, ttl_value()} | undefined.
ttl(#via{hparams = HParams}) ->
    case ersip_hparams:find(ttl, HParams) of
        {ok, TTL} -> {ok, TTL};
        not_found -> undefined
    end.

%% @doc Set ttl parameter.
-spec set_ttl(ttl_value(), via()) -> via().
set_ttl(TTLValue, #via{} = Via) ->
    set_param(ttl, TTLValue, Via).

%% @doc Make key that can be used to match transaction.
-spec make_key(via()) -> via_key().
make_key(#via{hparams = HParams} = Via) ->
    {sent_protocol_make_key(sent_protocol(Via)),
     sent_by_make_key(sent_by(Via)),
     via_params_make_key(HParams)}.

%% @doc Assemble Via as iolist().
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

%% @doc Assemble Via as binary().
-spec assemble_bin(via()) -> binary().
assemble_bin(#via{} = Via) ->
    iolist_to_binary(assemble(Via)).

%% @doc Parse single Via header
-spec parse(iolist()|binary()) -> {ok, via()} | {error, term()}.
parse(Binary) when is_binary(Binary) ->
    parse_via(Binary);
parse(IOList) ->
    parse_via(iolist_to_binary(IOList)).

%% @doc Make Via header from binary.
-spec make(binary() | raw()) -> via().
make(Bin) when is_binary(Bin) ->
    case parse_via(Bin) of
        {ok, Via} -> Via;
        {error, Reason} -> error(Reason)
    end.

%% @doc Represent via as raw SIP header.
-spec raw(via()) -> raw().
raw(#via{} = Via) ->
    {sent_protocol, Proto, Ver, Transp} = Via#via.sent_protocol,
    {sent_by, H, Prt} = Via#via.sent_by,
    Raw = #{protocol => Proto,
            version => Ver,
            transport => ersip_transport:assemble_upper(Transp),
            host => ersip_host:assemble_bin(H),
            port => Prt,
            params => ersip_hparams:raw(Via#via.hparams)
           },
    Opts = [{branch,   branch(Via),   fun(X) -> ersip_branch:assemble_bin(X) end},
            {maddr,    maddr(Via),    fun(X) -> ersip_host:assemble_bin(X) end},
            {received, received(Via), fun(X) -> ersip_host:assemble_bin(X) end},
            {ttl,      ttl(Via),      fun(X) -> X end},
            {rport,    rport(Via),    fun(X) -> X end}
           ],
    OptsKVP = [{K, F(V)} || {K, {ok, V}, F} <- Opts],
    maps:merge(maps:from_list(OptsKVP), Raw).


%%===================================================================
%% Internal implementation
%%===================================================================

-spec parse_via(binary()) -> {ok,via()} | {error, term()}.
parse_via(ViaBinary) ->
    case parse_via_with_rest(ViaBinary) of
        {ok, Via, <<>>} ->
            {ok, Via};
        {ok, _, _}      -> {error, {invalid_via, ViaBinary}};
        {error, Reason} -> {error, {invalid_via, Reason}}
    end.

-spec parse_via_with_rest(binary()) -> ersip_parser_aux:parse_result(via()).
parse_via_with_rest(ViaBinary) ->
    Parsers = [fun parse_sent_protocol/1,
               fun ersip_parser_aux:parse_lws/1,
               fun parse_sent_by/1,
               fun ersip_parser_aux:trim_lws/1,
               fun parse_params/1,
               fun ersip_parser_aux:trim_lws/1
              ],
    case ersip_parser_aux:parse_all(ViaBinary, Parsers) of
        {ok, [SentProtocol, _, SentBy, _, HParams, _], Rest} ->
            Via = #via{sent_protocol = SentProtocol,
                       sent_by       = SentBy,
                       hparams       = HParams},
            {ok, Via, Rest};
        {error, Reason} -> {error, {invalid_via, Reason}}
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
            <<HostPort:Pos/binary, Rest/binary>> = Binary,
            parse_sent_by_host_port(ersip_bin:trim_lws(HostPort), host, #{rest => Rest});
        nomatch ->
            case binary:match(Binary, <<",">>) of
                {Pos, 1} ->
                    <<HostPort:Pos/binary, Rest/binary>> = Binary,
                    parse_sent_by_host_port(ersip_bin:trim_lws(HostPort), host, #{rest => Rest});
                nomatch ->
                    parse_sent_by_host_port(ersip_bin:trim_lws(Binary), host, #{rest => <<>>})
            end
    end.

-spec parse_sent_by_host_port(binary() | [binary()], host | port | result, map()) -> ersip_parser_aux:parse_result().
parse_sent_by_host_port(<<$[, _/binary>> = IPv6RefPort, host, Acc) ->
    case binary:match(IPv6RefPort, <<"]">>) of
        nomatch ->
            {error, {invalid_ipv6_reference, IPv6RefPort}};
        {Pos, 1} ->
            Size = Pos+1,
            <<HostBin:Size/binary, Rest/binary>> = IPv6RefPort,
            case ersip_host:parse(HostBin) of
                {ok, Host, <<>>} ->
                    parse_sent_by_host_port(Rest, port, Acc#{host => Host});
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
parse_sent_by_host_port(Bin, port, _Acc) when is_binary(Bin) ->
    {error, {invalid_port, Bin}};
parse_sent_by_host_port([Bin], port, Acc) when is_binary(Bin) ->
    case parse_port_number(ersip_bin:trim_lws(Bin)) of
        {ok, PortNumber} ->
            parse_sent_by_host_port(<<>>, result, Acc#{port => PortNumber});
        {error, _} = Error ->
            Error
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

%% @private
-spec parse_params(binary()) -> ersip_parser_aux:parse_result(ersip_parser_aux:gen_param_list()).
parse_params(<<$;, Bin/binary>>) ->
    ersip_hparams:parse(fun parse_known/2, Bin);
parse_params(Bin) ->
    {ok, ersip_hparams:new(), Bin}.

%% via-params        =  via-ttl / via-maddr
%%                      / via-received / via-branch
%%                      / via-extension
-spec parse_known(binary(), binary()) -> ersip_hparams:parse_known_fun_result().
parse_known(<<"ttl">>, Value) ->
    %% 1*3DIGIT ; 0 to 255
    case ersip_parser_aux:parse_non_neg_int(Value) of
        {ok, TTL, <<>>}
          when TTL >= 0 andalso TTL =< 255
               ->          {ok, {ttl, TTL}};
        {ok, _, _} ->      {error, {invalid_ttl, Value}};
        {error, Reason} -> {error, {invalid_ttl, Reason}}
    end;
parse_known(<<"received">>, Value) ->
    %% "received" EQUAL (IPv4address / IPv6address)
    case ersip_host:parse(Value) of
        {ok, {ipv4, _} = Host, <<>>} -> {ok, {received, Host}};
        {ok, {ipv6, _} = Host, <<>>} -> {ok, {received, Host}};
        {ok, _, _} ->                   {error, {invalid_received, Value}};
        {error, Reason} ->              {error, {invalid_received, Reason}}
    end;
parse_known(<<"maddr">>, Value) ->
    %% "maddr" EQUAL host
    case ersip_host:parse(Value) of
        {ok, Host, <<>>} -> {ok, {maddr, Host}};
        {ok, _, _} ->       {error, {invalid_maddr, Value}};
        {error, Reason} ->  {error, {invalid_maddr, Reason}}
    end;
parse_known(<<"branch">>, Value) ->
    %% "branch" EQUAL token
    case ersip_parser_aux:check_token(Value) of
        true  -> {ok, {branch, ersip_branch:make(Value)}};
        false -> {error, {invalid_branch, Value}}
    end;
parse_known(<<"rport">>, <<>>) ->
    {ok, {rport, true}};
parse_known(<<"rport">>, Value) ->
    %% response-port = "rport" [EQUAL 1*DIGIT]
    case parse_port_number(Value) of
        {ok, Port}      -> {ok, {rport, Port}};
        {error, Reason} ->  {error, {invalid_rport, Reason}}
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

-spec assemble_received(ersip_host:host()) -> binary().
assemble_received(Value) ->
    iolist_to_binary(ersip_host:assemble_received(Value)).

-spec assemble_rport(rport_value()) -> binary().
assemble_rport(true) ->
    <<>>;
assemble_rport(Value) ->
    integer_to_binary(Value).

-spec assemble_ttl(ttl_value()) -> binary().
assemble_ttl(Value) ->
    integer_to_binary(Value).

-spec assemble_maddr(ersip_host:host()) -> binary().
assemble_maddr(Value) ->
    iolist_to_binary(ersip_host:assemble(Value)).


-spec validate_topmost_via(via(), binary()) -> ok | {error, term()}.
validate_topmost_via(#via{} = Via, <<>>) ->
    validate_topmost_via(Via);
validate_topmost_via(#via{} = Via, <<",", _/binary>>) ->
    validate_topmost_via(Via);
validate_topmost_via(#via{}, Rest) ->
    {error, {garbage_at_the_end, Rest}}.

-spec validate_topmost_via(via()) -> ok | {error, term()}.
validate_topmost_via(#via{sent_protocol = SentProtocol}) ->
    case SentProtocol of
        {sent_protocol, <<"SIP">>, <<"2.0">>, T} ->
            case ersip_transport:is_known_transport(T) of
                true ->  ok;
                false -> {error, {unknown_transport, T}}
            end;
        {sent_protocol, Proto, Version, _} ->
            {error, {unknown_protocol, Proto, Version}}
    end.

-spec parse_port_number(binary()) -> {ok, inet:port_number()} | {error, term()}.
parse_port_number(Bin) ->
    case ersip_parser_aux:parse_non_neg_int(Bin) of
       {ok, Int, <<>>} when Int > 0 andalso Int =< 65535 -> {ok, Int};
        _ -> {error, {invalid_port, Bin}}
    end.

