%%%
%%% Copyright (c) 2021 Dmitry Poroh
%%% All rights reserved.
%%% Distributed under the terms of the MIT License. See the LICENSE file.
%%%
%%% SIP URI Parser
%%%

-module(ersip_uri_sip_parser).

-export([parse_data/1,
         parse_and_add_param/2]).

-export_type([sip_uri_parse_error/0]).

%%===================================================================
%% Types
%%===================================================================

-type sip_uri_parse_result() :: {ok, ersip_uri_sip:sip_uri()} | {error, sip_uri_parse_error()}.
-type sip_uri_parse_error() :: {invalid_host, ersip_host:parse_error() | {garbage_at_the_end, binary()}}
                             | {invalid_ipv6_reference, binary()}
                             | {invalid_port, binary()}
                             | {invalid_maddr, binary()}
                             | {invalid_transport, ersip_transport:parse_error()}
                             | {invalid_user_param, binary()}
                             | {invalid_ttl, binary()}
                             | {invalid_parameter, binary(), binary()}.

%%===================================================================
%% API
%%===================================================================

-spec parse_data(binary()) -> sip_uri_parse_result().
parse_data(Bin) ->
    parse_usesrinfo(Bin).

-spec parse_and_add_param(binary(), ersip_uri_sip:sip_uri()) -> sip_uri_parse_result().
parse_and_add_param(Bin, SIPURI) ->
    do_parse_and_add_param(Bin, SIPURI).

%%===================================================================
%% Implementation of URI parser
%%===================================================================

-include("ersip_sip_abnf.hrl").

-spec parse_usesrinfo(binary()) -> sip_uri_parse_result().
parse_usesrinfo(Bin) ->
    case binary:split(Bin, <<"@">>) of
        [Userinfo, R] ->
            case patch_userinfo(Userinfo) of
                {ok, PatchedUserinfo} ->
                    parse_hostport(PatchedUserinfo, R);
                {error, _} = Error ->
                    Error
            end;
        [R] ->
            parse_hostport(undefined, R)
    end.

%% hostport         =  host [":" port]
-spec parse_hostport(binary() | undefined, binary()) -> sip_uri_parse_result().
parse_hostport(User, R) ->
    {HostPort, Params, Headers} = split_uri(R),
    MaybeSIPData =
        case split_hostport(HostPort) of
            {ok, {HostBin, <<>>}} ->
                case ersip_host:parse(HostBin) of
                    {ok, Host, <<>>} ->
                        {ok, ersip_uri_sip:new_parsed(User, Host, HostBin)};
                    {ok, _, End} ->
                        {error, {invalid_host, {garbage_at_the_end, End}}};
                    {error, Reason} ->
                        {error, {invalid_host, Reason}}
                end;
            {ok, {HostBin, PortBin}} ->
                case {ersip_host:parse(HostBin), parse_port(PortBin)} of
                    {{ok, Host, <<>>}, {ok, Port}} ->
                        URI = ersip_uri_sip:new_parsed(User, Host, HostBin),
                        URI1 = ersip_uri_sip:set_port(Port, URI),
                        {ok, URI1};
                    {{ok, _, End}, {ok, _}} ->
                        {error, {invalid_host, {garbage_at_the_end, End}}};
                    {{error, Reason}, _} ->
                        {error, {invalid_host, Reason}};
                    {_, {error, _} = Error} ->
                        Error
                end;
            {error, _} = Error ->
                Error
        end,
    MaybeSIPData1 = maybe_add_params(MaybeSIPData, Params),
    maybe_add_headers(MaybeSIPData1, Headers).

%% port           =  1*DIGIT
-spec parse_port(binary()) -> {ok, 0..65535} | {error, {invalid_port, binary()}}.
parse_port(Bin) ->
    case catch binary_to_integer(Bin) of
        Int when is_integer(Int) andalso Int >= 0 andalso Int =< 65535 ->
            {ok, Int};
        _ ->
            {error, {invalid_port, Bin}}
    end.

%% uri-parameters    =  *( ";" uri-parameter)
%% uri-parameter     =  transport-param / user-param / method-param
%%                      / ttl-param / maddr-param / lr-param / other-param
-spec maybe_add_params(sip_uri_parse_result(), binary()) -> sip_uri_parse_result().
maybe_add_params({error, _} = Err, _) ->
    Err;
maybe_add_params({ok, URIData}, <<>>) ->
    {ok, URIData};
maybe_add_params({ok, URIData}, ParamsBin) ->
    ParamsList = binary:split(ParamsBin, <<";">>, [global]),
    lists:foldl(fun(_, {error, _} = Err) ->
                        Err;
                   (Param, {ok, URIData1}) ->
                        do_parse_and_add_param(Param, URIData1)
                end,
                {ok, URIData},
                ParamsList).

-spec maybe_add_headers(sip_uri_parse_result(), binary()) -> sip_uri_parse_result().
maybe_add_headers({error, _} = Err, _) ->
    Err;
maybe_add_headers({ok, SIPURI}, <<>>) ->
    {ok, SIPURI};
maybe_add_headers({ok, SIPURI}, Headers) ->
    {ok, HeadersList, <<>>} = ersip_parser_aux:parse_kvps(fun uri_header_validator/2, <<"&">>, Headers),
    {ok, ersip_uri_sip:set_raw_headers(HeadersList, SIPURI)}.

%% userinfo         =  ( user / telephone-subscriber ) [":" password] "@"
%% password         =  *( unreserved / escaped / "&" / "=" / "+" / "$" / "," )
-spec patch_userinfo(binary()) -> {ok, binary()} | {error, term()}.
patch_userinfo(Bin) ->
    case binary:split(Bin, <<":">>) of
        [User, Password] ->
            case check_password(Password) of
                false -> {error, {bad_password, Password}};
                true ->
                    case patch_user(User, []) of
                        {error, _} = Error -> Error;
                        {ok, PUser} ->
                            {ok, <<PUser/binary, ":", Password/binary>>}
                    end
            end;
        [User] ->
            patch_user(User, [])
    end.

%% user             =  1*( unreserved / escaped / user-unreserved )
patch_user(<<>>, []) ->
    {error, empty_username};
patch_user(<<>>, Acc) ->
    {ok, iolist_to_binary(lists:reverse(Acc))};
patch_user(<<Char, R/binary>>, Acc) when ?is_unreserved(Char) orelse ?is_user_unreserved(Char) ->
    patch_user(R, [Char | Acc]);
patch_user(<<"%", A, B, R/binary>>, Acc) when ?is_HEXDIG(A) andalso ?is_HEXDIG(B) ->
    patch_user(R, [<<"%", A, B>> | Acc]);
patch_user(<<C, R/binary>>,  Acc) ->
    patch_user(R, [io_lib:format("%~2.16.0B", [C]) | Acc]).

-define(is_password_unreserved(X), (X =:= $& orelse X =:= $= orelse X =:= $+ orelse X =:= $$ orelse X =:= $,)).
%% password         =  *( unreserved / escaped / "&" / "=" / "+" / "$" / "," )
check_password(<<>>) ->
    true;
check_password(<<Char/utf8, R/binary>>) when ?is_unreserved(Char) orelse ?is_password_unreserved(Char) ->
    check_password(R);
check_password(<<"%", A/utf8, B/utf8, R/binary>>) when ?is_HEXDIG(A) andalso ?is_HEXDIG(B) ->
    check_password(R);
check_password(_) ->
    false.

-spec split_uri(binary()) -> {HostPort, Params, Headers} when
      HostPort :: binary(),
      Params   :: binary(),
      Headers  :: binary().
split_uri(Bin) ->
    case binary:match(Bin, <<";">>) of
        nomatch ->
            {HostPort, Headers} = split_headers(Bin),
            {HostPort, <<>>, Headers};
        {_, 1} ->
            {HostPort, Rest} = split_params(Bin),
            {Params, Headers} = split_headers(Rest),
            {HostPort, Params, Headers}
    end.

-spec split_hostport(binary()) -> {ok, {binary(), binary()}} | {error, sip_uri_parse_error()}.
split_hostport(<<$[, _/binary>> = IPv6RefPort) ->
    case binary:match(IPv6RefPort, <<"]">>) of
        nomatch ->
            {error, {invalid_ipv6_reference, IPv6RefPort}};
        {Pos, 1} when Pos + 1 =:= byte_size(IPv6RefPort) ->
            %% No port specified
            {ok, {IPv6RefPort, <<>>}};
        {Pos, 1} ->
            Size = Pos+1,
            <<Host:Size/binary, Rest/binary>> = IPv6RefPort,
            case Rest of
                <<$:, Port/binary>> when Port =/= <<>> ->
                    {ok, {Host, Port}};
                Else ->
                    {error, {invalid_port, Else}}
            end
    end;
split_hostport(IPOrHost) ->
    case binary:split(IPOrHost, <<":">>) of
        [H, P] ->
            {ok, {H, P}};
        [H] ->
            {ok, {H, <<>>}}
    end.

-spec split_headers(binary()) -> {binary(), binary()}.
split_headers(Bin) ->
    case binary:split(Bin, <<"?">>) of
        [Prefix, Headers] ->
            {Prefix, Headers};
        [Prefix] ->
            {Prefix, <<>>}
    end.


-spec split_params(binary()) -> {binary(), binary()}.
split_params(Bin) ->
    case binary:split(Bin, <<";">>) of
        [Prefix, Headers] ->
            {Prefix, Headers}
    end.

-spec uri_header_validator(binary(), binary() | novalue) -> {ok, {binary(), binary()}}.
uri_header_validator(Key, novalue) ->
    {ok, {Key, <<>>}};
uri_header_validator(Key, Value) ->
    {ok, {Key, Value}}.

%%===================================================================
%% Implementation of Parameters parser
%%===================================================================

%% @private
%% @doc Parse and add parameters described in RFC3261
-spec do_parse_and_add_param(binary(), ersip_uri_sip:sip_uri()) -> sip_uri_parse_result().
do_parse_and_add_param(Param, SIPData) ->
    Pair =
        case binary:split(Param, <<"=">>) of
            [Name] ->
                {ersip_bin:to_lower(ersip_uri_parser_aux:unquote_hex(Name)), Name, <<>>};
            [Name, Value] ->
                {ersip_bin:to_lower(ersip_uri_parser_aux:unquote_hex(Name)), Name, Value}
        end,

    case Pair of
        {<<"transport">>, _, V} ->
            %% transport-param   =  "transport="
            %%                      ( "udp" / "tcp" / "sctp" / "tls"
            %%                      / other-transport)
            %% other-transport   =  token
            %%
            case ersip_transport:parse(V) of
                {error, Reason} ->
                    {error, {invalid_transport, Reason}};
                {ok, T} ->
                    {ok, ersip_uri_sip:set_transport(T, SIPData)}
            end;

        {<<"maddr">>, _, A} ->
            %% maddr-param       =  "maddr=" host
            case ersip_host:parse(A) of
                {ok, Host, <<>>} ->
                    {ok, ersip_uri_sip:set_maddr(Host, SIPData)};
                _ ->
                    {error, {invalid_maddr, A}}
            end;

        {<<"user">>, _, U} ->
            %%  user-param        =  "user=" ( "phone" / "ip" / other-user)
            case ersip_bin:to_lower(U) of
                <<"phone">> ->
                    {ok, ersip_uri_sip:set_user_param(phone, SIPData)};
                <<"ip">> ->
                    {ok, ersip_uri_sip:set_user_param(ip, SIPData)};
                _ ->
                    case ersip_parser_aux:check_token(U) of
                        true ->
                            {ok, ersip_uri_sip:set_user_param(U, SIPData)};
                        false ->
                            {error, {invalid_user_param, U}}
                    end
            end;

        {<<"lr">>, _, _} ->
            {ok, ersip_uri_sip:set_loose_router(true, SIPData)};

        {<<"ttl">>, _, TTLBin} ->
            case catch binary_to_integer(TTLBin) of
                TTL when is_integer(TTL) andalso TTL >= 0 andalso TTL =< 255 ->
                    {ok, ersip_uri_sip:set_ttl(TTL, SIPData)};
                _ ->
                    {error, {invalid_ttl, TTLBin}}
            end;

        {Other, OrigName, OtherVal} ->
            case is_pname(OrigName) andalso is_pvalue(OtherVal) of
                true ->
                    {ok, ersip_uri_sip:set_param(Other, OtherVal, SIPData)};
                false ->
                    {error, {invalid_parameter, Other, OtherVal}}
            end
    end.

-spec is_pname(binary()) -> boolean().
is_pname(<<>>) ->
    false;
is_pname(Val) ->
    is_paramchar_string(Val).

-spec is_pvalue(binary()) -> boolean().
is_pvalue(<<>>) ->
    true;
is_pvalue(Val) ->
    is_paramchar_string(Val).

%% paramchar         =  param-unreserved / unreserved / escaped
%% param-unreserved  =  "[" / "]" / "/" / ":" / "&" / "+" / "$"
-define(is_param_unreserved(X),  (X == $[ orelse X == $]
                          orelse X == $/ orelse X == $:
                          orelse X == $& orelse X == $+
                          orelse X == $$)).


-spec is_paramchar_string(binary()) -> boolean().
is_paramchar_string(<<>>) ->
    true;
is_paramchar_string(<<C:8, Rest/binary>>) when ?is_unreserved(C)
                                        orelse ?is_param_unreserved(C) ->
    is_paramchar_string(Rest);
is_paramchar_string(<<$%, H1:8, H2:8, Rest/binary>>) when ?is_HEXDIG(H1) andalso ?is_HEXDIG(H2) ->
    %% escaped     =  "%" HEXDIG HEXDIG
    is_paramchar_string(Rest);
is_paramchar_string(_) ->
    false.
