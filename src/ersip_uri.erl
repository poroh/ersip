%%
%% Copyright (c) 2017 Dmitry Poroh
%% All rights reserved.
%% Distributed under the terms of the MIT License. See the LICENSE file.
%%
%% SIP URI
%%

-module(ersip_uri).

-export([ make/1, parse/1, set_param/3 ]).

-include("ersip_uri.hrl").
-include("ersip_sip_abnf.hrl").

-type uri_param_name() :: transport
                        | user
                        | method
                        | ttl
                        | maddr
                        | lr
                        | binary().

-type uri_part() :: sip
                  | sips
                  | { user, binary() }
                  | { host, ersip_host:host() }
                  | { port, 0..65535 }.

-export_type([ uri/0 ]).

%%%===================================================================
%%% API
%%%===================================================================

-spec make(Parts :: [ list(uri_part()) ]) -> uri().
make(Parts) ->
    Init = #uri{ host = { ipv4, { 0, 0, 0, 0 } } },
    lists:foldl(fun(Option, URI) ->
                       set_part(Option, URI)
                end,
                Init,
                Parts).

%% @doc Parse URI from the binary
%% SIP-URI          =  "sip:" [ userinfo ] hostport
%%                     uri-parameters [ headers ]
%% SIPS-URI         =  "sips:" [ userinfo ] hostport
%%                     uri-parameters [ headers ]
-spec parse(binary()) -> { ok, uri() } | { error, { einval, atom() } }.
parse(<<"sip:", R/binary>>) ->
    parse_usesrinfo(sip, R);
parse(<<"sips:", R/binary>>) ->
    parse_usesrinfo(sips, R);
parse(_) ->
    { error, { einval, invalid_scheme } }.

%% @doc set paramter of the URI
-spec set_param(uri_param_name(), term(), uri()) -> uri().
set_param(ParamName, Value, #uri{ params = P } = URI) ->
    URI#uri{ params = P#{ ParamName => Value } }.

%% @doc set paramter of the URI
-spec set_part(uri_part(), uri()) -> uri().
set_part(sip,                #uri{} = URI) -> URI#uri{ scheme = sip };
set_part(sips,               #uri{} = URI) -> URI#uri{ scheme = sips };
set_part({ user, U } = User, #uri{} = URI) when is_binary(U) -> URI#uri{ user = User };
set_part({ port, P },        #uri{} = URI) when is_integer(P) -> URI#uri{ port = P };
set_part({ host, H },        #uri{} = URI) ->
    case ersip_host:is_host(H) of
        true ->
            URI#uri{ host = H };
        false ->
            error(badarg)
    end;
set_part(_, _) ->
    error(badarg).


%%%===================================================================
%%% Internal implementation
%%%===================================================================

-spec parse_usesrinfo(Scheme, binary()) -> { ok, uri() } | { error, { einval, atom() } } when
      Scheme :: sip | sips.
parse_usesrinfo(Scheme, Bin) ->
    case binary:split(Bin, <<"@">>) of
        [ Userinfo, R ] ->
            case check_userinfo(Userinfo) of
                true ->
                    parse_hostport(Scheme, { user, Userinfo }, R);
                _ ->
                    { error, { einval, userinfo } }
            end;
        [ R ] ->
            parse_hostport(Scheme, undefined, R)
    end.

%% hostport         =  host [ ":" port ]
-spec parse_hostport(Scheme, User, binary()) -> { ok, uri() } | { error, { einval, atom() } } when
      Scheme :: sip | sips,
      User   :: {user, binary() } | undefined.
parse_hostport(Scheme, User, R) ->
    { HostPort, Params } =
        case binary:split(R, <<";">>) of
            [ H, P ] ->
                { H, P };
            [ H ] ->
                { H, <<>> }
        end,
    URI =
        case binary:split(HostPort, <<":">>) of
            [ HostBin, PortBin ] ->
                case { ersip_host:parse(HostBin), parse_port(PortBin) } of
                    { { ok, Host }, { ok, Port } } ->
                        #uri{ scheme = Scheme,
                              user   = User,
                              host   = Host,
                              port   = Port
                            };
                    _ ->
                        { error, { einval, hostport } }
                end;
            [ HostBin ] ->
                case ersip_host:parse(HostBin) of
                    { ok, Host } ->
                        #uri{ scheme = Scheme,
                              user   = User,
                              host   = Host
                            };
                    _ ->
                        { error, { einval, host } }
                end
        end,
    maybe_add_params(URI, Params).

%% port           =  1*DIGIT
-spec parse_port(binary()) -> { ok, 0..65535 } |  { error, { einval, port } }.
parse_port(Bin) ->
    case catch binary_to_integer(Bin) of
        Int when is_integer(Int) andalso Int >= 0 andalso Int =< 65535 ->
            { ok, Int };
        _ ->
            { error, { einval, port } }
    end.

%% uri-parameters    =  *( ";" uri-parameter)
%% uri-parameter     =  transport-param / user-param / method-param
%%                      / ttl-param / maddr-param / lr-param / other-param
-spec maybe_add_params(UriOrError, binary()) -> { ok, uri() } | { error, { einval, atom() } } when
      UriOrError :: uri()
                  | { error, { einval, atom() } }.
maybe_add_params({error, _ } = Err, _) ->
    Err;
maybe_add_params(#uri{} = URI, <<>>) ->
    { ok, URI };
maybe_add_params(#uri{} = URI, ParamsBin) ->
    ParamsList = binary:split(ParamsBin, <<";">>),
    R =
        lists:foldl(fun(_, { error, _ } = Err) ->
                            Err;
                       (Param, #uri{} = URI_) ->
                            parse_and_add_param(Param, URI_)
                    end,
                    URI,
                    ParamsList),
    case R of
        #uri{} ->
            { ok, R };
        Err ->
            Err
    end.

%% @private
%% @doc Parse and add parameters described in RFC3261
-spec parse_and_add_param(binary(), uri()) -> { ok, uri() } | { error, { einval, atom() } }.
parse_and_add_param(Param, URI) ->
    Pair =
        case binary:split(Param, <<"=">>) of
            [ Name ] ->
                { ersip_bin:to_lower(Name), <<>> };
            [ Name, Value ] ->
                { ersip_bin:to_lower(Name), Value }
        end,

    case Pair of
        { <<"transport">>, V } ->
            %% transport-param   =  "transport="
            %%                      ( "udp" / "tcp" / "sctp" / "tls"
            %%                      / other-transport)
            %% other-transport   =  token
            %%
            case parse_transport(V) of
                { error, _ } = Err ->
                    Err;
                T ->
                    set_param(transport, T, URI)
            end;

        { <<"maddr">>, A } ->
            %% maddr-param       =  "maddr=" host
            case ersip_host:parse(A) of
                { ok, Host } ->
                    set_param(maddr, Host, URI);
                _ ->
                    { error, { einval, maddr } }
            end;

        { <<"user">>, U } ->
            %%  user-param        =  "user=" ( "phone" / "ip" / other-user)
            case ersip_bin:to_lower(U) of
                <<"phone">> ->
                    set_param(user, phone, URI);
                <<"ip">> ->
                    set_param(user, ip, URI);
                _ ->
                    case check_token(U) of
                        true ->
                            set_param(user, U, URI);
                        false ->
                            { error, { einval, user_param } }
                    end
            end;

        { <<"lr">>, _ } ->
            set_param(lr, true, URI);

        { <<"ttl">>, TTLBin } ->
            case catch binary_to_integer(TTLBin) of
                TTL when is_integer(TTL) andalso TTL >= 0 andalso TTL =< 255 ->
                    set_param(ttl, TTL, URI);
                _ ->
                    { error, { einval, ttl } }
            end;

        { Other, OtherVal } ->
            %% TODO check Other & OtherVal for compliance.
            set_param(Other, OtherVal, URI)
    end.

-spec parse_transport(binary()) -> transport() | { error, { einval, transport } }.
parse_transport(V) ->
    case ersip_bin:to_lower(V) of
        <<"tcp">> -> { transport, tcp };
        <<"udp">> -> { transport, udp };
        <<"tls">> -> { transport, tls };
        <<"wss">> -> { transport, wss };
        <<"ws">>  -> { transport, ws  };
        Bin ->
            case check_token(Bin) of
                true  -> { other_transport, Bin };
                false -> { error, { einval, transport } }
            end
    end.

%% userinfo         =  ( user / telephone-subscriber ) [ ":" password ] "@"
%% password         =  *( unreserved / escaped / "&" / "=" / "+" / "$" / "," )
-spec check_userinfo(binary()) -> boolean().
check_userinfo(Bin) ->
    case binary:split(Bin, <<":">>) of
        [ User, Password ] ->
            check_user(User, start) andalso check_password(Password);
        [ User ] ->
            check_user(User, start)
    end.

%% user             =  1*( unreserved / escaped / user-unreserved )
check_user(<<>>, start) ->
    false;
check_user(<<>>, rest) ->
    true;
check_user(<<Char/utf8, R/binary>>, _) when ?is_unreserved(Char) orelse ?is_user_unreserved(Char) ->
    check_user(R, rest);
check_user(<<"%", A/utf8, B/utf8, R/binary>>, _) when ?is_HEXDIG(A) andalso ?is_HEXDIG(B) ->
    check_user(R, rest);
check_user(_, _) ->
    false.

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

check_token(Bin) ->
    ersip_parser_aux:check_token(Bin).
