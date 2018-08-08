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
         params/1,
         set_param/3,
         branch/1,
         sent_by/1,
         sent_by_key/1,
         make_key/1,
         assemble/1,
         parse/1
        ]).

-export_type([via/0,
              sent_by/0
             ]).

%%%===================================================================
%%% Types
%%%===================================================================

-record(via, {sent_protocol  :: sent_protocol(),
              sent_by        :: internal_sent_by(),
              via_params     :: via_params()
             }).
-type via()           :: #via{}.
-type sent_protocol() :: {sent_protocol, Protocol :: binary(), ProtocolVersion :: binary(), ersip_transport:transport()}.
-type sent_by()       :: {sent_by, ersip_host:host(), Port :: ersip_transport:port_number()}.
-type internal_sent_by() :: {sent_by, ersip_host:host(), Port :: ersip_transport:port_number() | default_port}.
-type via_params()    :: map().
-type known_via_params() :: branch
                          | maddr
                          | received
                          | ttl
                          | rport.

%%%===================================================================
%%% API
%%%===================================================================

-spec new(ersip_host:host(), ersip_transport:port_number(), ersip_transport:transport()) -> via().
new(Address, Port, Transport) ->
    #via{sent_protocol = {sent_protocol, <<"SIP">>, <<"2.0">>, Transport},
         sent_by = {sent_by, Address, Port},
         via_params = #{}
        }.

-spec new(ersip_host:host(), ersip_transport:port_number(), ersip_transport:transport(), ersip_branch:branch()) -> via().
new(Address, Port, Transport, Branch) ->
    #via{sent_protocol = {sent_protocol, <<"SIP">>, <<"2.0">>, Transport},
         sent_by = {sent_by, Address, Port},
         via_params = #{branch => Branch}
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

-spec params(via()) -> via_params().
params(#via{via_params = VP}) ->
    VP.

-spec set_param(ParamName, Value, via()) -> via() when
      ParamName :: known_via_params() | binary(),
      Value     :: binary()
                 | ersip_host:host().
set_param(received, Value, Via) when is_binary(Value) ->
    case ersip_host:parse(Value) of
        {ok, Host} ->
            set_param(received, Host, Via);
        {error, _} = Error ->
            error(Error)
    end;
set_param(received, {Type, _} = Value , #via{via_params = VP} = Via)
  when Type =:= ipv4 orelse Type =:= ipv6 ->
    Via#via{via_params = VP#{received => Value}};
set_param(received, Value, _) ->
    error({error, {bad_received_via_param, Value}});
set_param(rport, Value, #via{via_params = VP} = Via)
  when is_integer(Value) andalso Value >= 1 andalso Value =< 65535 ->
    Via#via{via_params = VP#{rport => Value}};
set_param(rport, true, #via{via_params = VP} = Via) ->
    Via#via{via_params = VP#{rport => true}};
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

-spec branch(via()) -> ersip_branch:branch() | undefined.
branch(#via{} = Via) ->
    Params = params(Via),
    maps:get(branch, Params, undefined).

-spec make_key(via()) -> via().
make_key(#via{} = Via) ->
    #via{sent_protocol = sent_protocol_make_key(sent_protocol(Via)),
         sent_by       = sent_by_make_key(sent_by(Via)),
         via_params    = via_params_make_key(params(Via))
        }.

-spec assemble(via()) -> iolist().
assemble(#via{} = Via) ->
    #via{
       sent_protocol =
           {sent_protocol, Protocol, ProtocolVersion, Transport},
       sent_by    = {sent_by, Host, Port},
       via_params = Params
      } = Via,
    [Protocol, $/, ProtocolVersion, $/, ersip_transport:assemble_upper(Transport),
     <<" ">>, ersip_host:assemble(Host),
     case Port of
         default_port ->
             [];
         Port ->
             [$:, integer_to_binary(Port)]
     end,
     assemble_params(Params)
    ].

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
        {ok, [SentProtocol, _, SentBy, _, ViaParams], _} ->
            Via = #via{sent_protocol = SentProtocol,
                       sent_by       = SentBy,
                       via_params    = ViaParams},
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
                {ok, Host} ->
                    parse_sent_by_host_port(Rest, port, Acc#{host => Host});
                {error, _} = Err ->
                    Err
            end
    end;
parse_sent_by_host_port(Binary, host, Acc) ->
    [HostBin | MayBePort] = binary:split(Binary, <<":">>),
    case ersip_host:parse(HostBin) of
        {ok, Host} ->
            parse_sent_by_host_port(MayBePort, port, Acc#{host => Host});
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

-spec parse_via_params(binary()) -> ersip_parser_aux:parse_result(via_params()).
parse_via_params(<<$;, Rest/binary>>) ->
    do_parse_via_params(Rest);
parse_via_params(Binary) ->
    do_parse_via_params(Binary).

-spec do_parse_via_params(binary()) -> ersip_parser_aux:parse_result(via_params()).
do_parse_via_params(Binary) ->
    case ersip_parser_aux:parse_kvps(fun via_params_val/2, <<";">>, Binary) of
        {ok, L, Rest} ->
            {ok, maps:from_list(L), Rest};
        {error, _} = Error ->
            Error
    end.

%% via-params        =  via-ttl / via-maddr
%%                      / via-received / via-branch
%%                      / via-extension
-spec via_params_val(Key, Value) -> Result when
      Key    :: binary(),
      Value  :: binary(),
      Result :: {ok, {ResultKey, ResultVal}}
              | {error, term()},
      ResultKey :: known_via_params() | binary(),
      ResultVal :: non_neg_integer()
                 | ersip_host:host()
                 | binary().
via_params_val(<<"rport">>, novalue) ->
    {ok, {rport, true}};
via_params_val(Key, novalue) ->
    {ok, {Key, true}};
via_params_val(Key, Value) ->
    KeyT = ersip_bin:to_lower(ersip_bin:trim_head_lws(Key)),
    ValT = ersip_bin:trim_head_lws(Value),
    via_params_val_impl(KeyT, ValT).

-spec via_params_val_impl(Key, Value) -> Result when
      Key    :: binary(),
      Value  :: binary(),
      Result :: {ok, {ResultKey, ResultVal}}
              | {error, term()},
      ResultKey :: known_via_params() | binary(),
      ResultVal :: non_neg_integer()
                 | ersip_host:host()
                 | binary().
via_params_val_impl(<<"ttl">>, Value) ->
    %% 1*3DIGIT ; 0 to 255
    case ersip_parser_aux:parse_non_neg_int(Value) of
        {ok, TTL, <<>>} when TTL >= 0 andalso TTL =< 255 ->
            {ok, {ttl, TTL}};
        _ ->
            {error, {invalid_ttl, Value}}
    end;
via_params_val_impl(<<"received">>, Value) ->
    %% "received" EQUAL (IPv4address / IPv6address)
    case ersip_host:parse(Value) of
        {ok, {ipv4, _} = Host} ->
            {ok, {received, Host}};
        {ok, {ipv6, _} = Host} ->
            {ok, {received, Host}};
        _ ->
            {error, {invalid_received, Value}}
    end;
via_params_val_impl(<<"maddr">>, Value) ->
    %% "received" EQUAL (IPv4address / IPv6address)
    case ersip_host:parse(Value) of
        {ok, Host} ->
            {ok, {maddr, Host}};
        _ ->
            {error, {invalid_maddr, Value}}
    end;
via_params_val_impl(<<"branch">>, Value) ->
    %% "branch" EQUAL token
    case ersip_parser_aux:parse_token(Value) of
        {ok, Branch, <<>>} ->
            {ok, {branch, ersip_branch:make(Branch)}};
        _ ->
            {error, {invalid_branch, Value}}
    end;
via_params_val_impl(<<"rport">>, Value) ->
    %% response-port = "rport" [EQUAL 1*DIGIT]
    case ersip_transport:parse_port_number(Value) of
        {ok, Port, <<>>} ->
            {ok, {rport, Port}};
        _ ->
            {error, {invalid_rport, Value}}
    end;
via_params_val_impl(Key, Value) ->
    case ersip_parser_aux:parse_gen_param_value(Value) of
        {ok, ParsedValue, <<>>} ->
            {ok, {Key, ParsedValue}};
        _ ->
            {error, {invalid_gen_param, {Key, Value}}}
    end.

-spec sent_protocol_make_key(sent_protocol()) -> sent_protocol().
sent_protocol_make_key(Protocol) ->
    Protocol.

-spec sent_by_make_key(sent_by()) -> sent_by().
sent_by_make_key({sent_by, Host, Port}) ->
    {sent_by, ersip_host:make_key(Host), Port}.

-spec via_params_make_key(via_params()) -> via_params().
via_params_make_key(Params) ->
    L = maps:to_list(Params),
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

-spec assemble_params(via_params()) -> [iolist()].
assemble_params(Params) ->
    lists:map(fun assemble_param/1,
              maps:to_list(Params)).

-spec assemble_param({Name, Value}) -> iolist() when
      Name :: known_via_params()
            | binary(),
      Value :: term().
assemble_param({received, Value}) ->
    [<<";received=">>, ersip_host:assemble(Value)];
assemble_param({rport, true}) ->
    <<";rport">>;
assemble_param({rport, Value}) ->
    [<<";rport=">>, integer_to_binary(Value)];
assemble_param({ttl, Value}) ->
    [<<";ttl=">>, integer_to_binary(Value)];
assemble_param({maddr, Value}) ->
    [<<";maddr=">>, ersip_host:assemble(Value)];
assemble_param({branch, Value}) ->
    [<<";branch=">>, ersip_branch:assemble(Value)];
assemble_param({Name, true}) ->
    [<<";", Name/binary>>];
assemble_param({Name, Value}) ->
    <<";", Name/binary, "=", Value/binary>>.
