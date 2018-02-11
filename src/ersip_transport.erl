%%
%% Copyright (c) 2018 Dmitry Poroh
%% All rights reserved.
%% Distributed under the terms of the MIT License. See the LICENSE file.
%%
%% SIP transport
%%

-module(ersip_transport).

-export([ make/1,
          parse/1,
          parse_port_number/1,
          default_port/1
        ]).

%%%===================================================================
%%% Types
%%%===================================================================

-type transport_atom() :: udp | tcp | tls | ws | wss.
-type transport() :: { transport, transport_atom() }
                   | { other_transport, binary() }.

-type port_number() :: 0..65535 | { default_port, transport() }.
-export_type([ transport/0,
               port_number/0 ]).

%%%===================================================================
%%% API
%%%===================================================================

-spec make(binary() | transport_atom()) -> transport().
make(V) ->
    case parse(V) of
        { ok, T } ->
            T;
        _ ->
            error(badarg)
    end.

-spec parse(binary() | transport_atom()) -> { ok, transport() } | { error, { einval, transport } }.
parse(V) when is_binary(V) ->
    case parse_bin(V) of
        { error, _ } = Error ->
            Error;
        T ->
            { ok, T }
    end;
parse(V) when V =:= tcp; V =:= udp; V =:= tls; V =:= wss; V =:= ws ->
    { ok, { transport, V } };
parse(V) when is_atom(V) ->
    { error, { bad_transport_atom, V } }.

-spec parse_port_number(binary()) -> ersip_parser_aux:parse_result(port_number()).
parse_port_number(Bin) ->
    case ersip_parser_aux:parse_non_neg_int(Bin) of
        { ok, Int, _ } = R when Int > 0 andalso Int =< 65535 ->
            R;
        _ ->
            { error, { invalid_port, Bin } }
    end.

-spec default_port(transport()) -> 0..65535 | { default_port, transport() }.
default_port({transport, tcp}) ->
    5060; %% RFC 3261 19.1.2
default_port({transport, udp}) ->
    5060; %% RFC 3261 19.1.2
default_port({transport, tls}) ->
    5061; %% RFC 3261 19.1.2
default_port({transport, ws}) ->
    80;   %% RFC 7118 5.5
default_port({transport, wss}) ->
    443;  %% RFC 7118 5.5
default_port({other_transport, _ } = T) ->
    { default_port, T}.

%%%===================================================================
%%% Internal implementation
%%%===================================================================

-spec parse_bin(binary()) -> transport().
parse_bin(V) ->
    case ersip_bin:to_lower(V) of
        <<"tcp">> -> { transport, tcp };
        <<"udp">> -> { transport, udp };
        <<"tls">> -> { transport, tls };
        <<"wss">> -> { transport, wss };
        <<"ws">>  -> { transport, ws  };
        Bin ->
            case ersip_parser_aux:check_token(Bin) of
                true  -> { other_transport, Bin };
                false -> { error, { einval, transport } }
            end
    end.
