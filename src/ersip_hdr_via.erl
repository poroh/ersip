%%
%% Copyright (c) 2018 Dmitry Poroh
%% All rights reserved.
%% Distributed under the terms of the MIT License. See the LICENSE file.
%%
%% SIP Via header
%%

-module(ersip_hdr_via).

-export([ topmost_via/1,
          sent_protocol/1,
          params/1,
          sent_by/1 ]).


%%%===================================================================
%%% Types
%%%===================================================================

-record(via, { sent_protocol  :: sent_protocol(),
               sent_by        :: internal_sent_by(),
               via_params     :: via_params()
             }).
-type via()           :: #via{}.
-type sent_protocol() :: { sent_protocol, Protocol :: binary(), ProtocolVersion :: binary(), ersip_transport:transport() }.
-type branch()        :: { branch, binary() }.
-type sent_by()       :: { sent_by, ersip_host:host(), Port :: ersip_transport:port_number() }.
-type internal_sent_by() :: { sent_by, ersip_host:host(), Port :: ersip_transport:port_number() | default_port }.
-type via_params()    :: #{}.
-type known_via_params() :: branch
                          | maddr
                          | received
                          | ttl.

-export_type([ via/0, branch/0, sent_by/0 ]).

%%%===================================================================
%%% API
%%%===================================================================

-spec topmost_via(ersip_hdr:header()) -> Result when
      Result :: { ok, via() }
              | { error, Error },
      Error  :: no_via.
topmost_via(Header) ->
    case ersip_hdr:raw_values(Header) of
        [] ->
            { error, no_via };
        [ TopVia | _ ]  ->
            parse_via(iolist_to_binary(TopVia))
    end.

-spec sent_protocol(via()) -> sent_protocol().
sent_protocol(#via{sent_protocol = Sp}) ->
    Sp.

-spec params(via()) -> via_params().
params(#via{via_params = VP}) ->
    VP.

-spec sent_by(via()) -> sent_by().
sent_by(#via{sent_by = {sent_by,Host,default_port}} = Via) ->
    {sent_protocol, _, _, Transport} = sent_protocol(Via),
    {sent_by, Host, ersip_transport:default_port(Transport) };
sent_by(#via{sent_by = SentBy}) ->
    SentBy.

%%%===================================================================
%%% Internal implementation
%%%===================================================================

-spec parse_via(binary()) -> {ok,via()} | {error, term()}.
parse_via(ViaBinary) ->
    Parsers = [ fun parse_sent_protocol/1,
                fun ersip_parser_aux:parse_lws/1,
                fun parse_sent_by/1,
                fun parse_via_params/1 ],
    case ersip_parser_aux:parse_all(ViaBinary, Parsers) of
        { ok, [ SentProtocol, _, SentBy, ViaParams ], _ } ->
            Via = #via{ sent_protocol = SentProtocol,
                        sent_by       = SentBy,
                        via_params    = ViaParams },
            { ok, Via };
        { error, _ } = Error ->
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
    SEPParser = ersip_parser_aux:make_sep_parser($/),
    Parsers = [ fun ersip_parser_aux:parse_token/1,
                SEPParser,
                fun ersip_parser_aux:parse_token/1,
                SEPParser,
                fun parse_transport/1
              ],
    case ersip_parser_aux:parse_all(Binary, Parsers) of
        { ok, [ ProtocolName, _, ProtocolVersion, _, Transport ], Rest } ->
            { ok, { sent_protocol, ProtocolName, ProtocolVersion, Transport }, Rest };
        { error, _ } = Error ->
            Error
    end.

%% sent-by           =  host [ COLON port ]
%% this implementation is not pure. It uses sent-by context in via
%% header (expects that either EOL or SEMI occur after sent-by).
-spec parse_sent_by(binary()) -> ersip_parser_aux:parse_result().
parse_sent_by(Binary) ->
    case binary:match(Binary, <<";">>) of
        { Pos, 1 } ->
            HostPort = binary:part(Binary, { 0, Pos }),
            Rest = binary:part(Binary, Pos, byte_size(Binary) - Pos),
            parse_sent_by_host_port(HostPort, host, #{ rest => Rest });
        nomatch ->
            parse_sent_by_host_port(Binary, host, #{ rest => <<>> })
    end.

parse_sent_by_host_port(Binary, host, Acc) ->
    [ HostBin | MayBePort ] = binary:split(Binary, <<":">>),
    case ersip_host:parse(HostBin) of
        { ok, Host } ->
            parse_sent_by_host_port(MayBePort, port, Acc#{ host => Host });
        { error, _ } = Err ->
            Err
    end;
parse_sent_by_host_port([], port, Acc) ->
    parse_sent_by_host_port(<<>>, result, Acc);
parse_sent_by_host_port([ Bin ], port, Acc) when is_binary(Bin) ->
    case ersip_transport:parse_port_number(Bin) of
        { ok, PortNumber, <<>> } ->
            parse_sent_by_host_port(<<>>, result, Acc#{ port => PortNumber });
        _ ->
            { error, { invalid_port, Bin } }
    end;
parse_sent_by_host_port(_, result, #{ rest := Rest, host := Host } = Acc) ->
    Port = maps:get(port, Acc, default_port),
    { ok, { sent_by, Host, Port }, Rest }.

-spec parse_transport(binary()) -> ersip_parser_aux:parse_result(ersip_transport:transport()).
parse_transport(Binary) ->
    case ersip_parser_aux:parse_token(Binary) of
        { ok, Transp, Rest } ->
            { ok, T } = ersip_transport:parse(Transp),
            { ok, T, Rest };
        { error, _ } = Error ->
            Error
    end.

parse_via_params(Binary) ->
    case ersip_parser_aux:parse_kvps(fun via_params_val/2, <<";">>, Binary) of
        { ok, L, Rest } ->
            { ok, maps:from_list(L), Rest };
        { error, _ } = Error ->
            Error
    end.

%% via-params        =  via-ttl / via-maddr
%%                      / via-received / via-branch
%%                      / via-extension
-spec via_params_val(Key, Value) -> Result when
      Key    :: binary(),
      Value  :: binary(),
      Result :: { ok, { ResultKey, ResultVal } }
              | { error, term() },
      ResultKey :: known_via_params() | binary(),
      ResultVal :: non_neg_integer()
                 | ersip_host:host()
                 | binary().
via_params_val(Key, novalue) ->
    { ok, { Key, true } };
via_params_val(Key, Value) ->
    KeyT = ersip_bin:to_lower(ersip_bin:trim_head_lws(Key)),
    ValT = ersip_bin:trim_head_lws(Value),
    via_params_val_impl(KeyT, ValT).

-spec via_params_val_impl(Key, Value) -> Result when
      Key    :: binary(),
      Value  :: binary(),
      Result :: { ok, { ResultKey, ResultVal } }
              | { error, term() },
      ResultKey :: known_via_params() | binary(),
      ResultVal :: non_neg_integer()
                 | ersip_host:host()
                 | binary().
via_params_val_impl(<<"ttl">>, Value) ->
    %% 1*3DIGIT ; 0 to 255
    case ersip_parser_aux:parse_non_neg_int(Value) of
        { ok, TTL, <<>> } when TTL >= 0 andalso TTL =< 255 ->
            { ok, { ttl, TTL } };
        _ ->
            { error, { invalid_ttl, Value } }
    end;
via_params_val_impl(<<"received">>, Value) ->
    %% "received" EQUAL (IPv4address / IPv6address)
    case ersip_host:parse(Value) of
        { ok, { ipv4, _ } = Host } ->
            { ok, { received, Host } };
        { ok, { ipv6, _ } = Host } ->
            { ok, { received, Host } };
        _ ->
            { error, { invalid_received, Value } }
    end;
via_params_val_impl(<<"maddr">>, Value) ->
    %% "received" EQUAL (IPv4address / IPv6address)
    case ersip_host:parse(Value) of
        { ok, Host } ->
            { ok, { maddr, Host } };
        _ ->
            { error, { invalid_maddr, Value } }
    end;
via_params_val_impl(<<"branch">>, Value) ->
    %% "branch" EQUAL token
    case ersip_parser_aux:parse_token(Value) of
        { ok, Branch, <<>> } ->
            { ok, { branch, Branch } };
        _ ->
            { error, { invalid_branch, Value } }
    end;
via_params_val_impl(Key, Value) ->
    case ersip_parser_aux:parse_gen_param_value(Value) of
        { ok, ParsedValue, <<>> } ->
            { ok, { Key, ParsedValue } };
        _ ->
            { error, { invalid_gen_param, { Key, Value } } }
    end.
