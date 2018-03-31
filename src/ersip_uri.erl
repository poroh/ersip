%%
%% Copyright (c) 2017, 2018 Dmitry Poroh
%% All rights reserved.
%% Distributed under the terms of the MIT License. See the LICENSE file.
%%
%% SIP URI
%%

-module(ersip_uri).

-export([ scheme/1,
          make/1,
          make_key/1,
          parse/1,
          assemble/1,
          params/1,
          set_param/3,
          clear_not_allowed_parts/2
        ]).
-export_type([ uri/0, scheme/0 ]).


-include("ersip_uri.hrl").
-include("ersip_sip_abnf.hrl").

%%%===================================================================
%%% Types
%%%===================================================================
-type uri_param_name() :: transport
                        | user
                        | method
                        | ttl
                        | maddr
                        | lr
                        | binary().

-type uri_part() :: scheme()
                  | { user, binary() }
                  | { host, ersip_host:host() }
                  | { port, 0..65535 }.

-type scheme() :: uri_scheme().

%%%===================================================================
%%% API
%%%===================================================================

-spec scheme(uri()) -> scheme().
scheme(#uri{ scheme = S }) ->
    S.

-spec make(Parts :: [ list(uri_part()) ]) -> uri().
make(Bin) when is_binary(Bin) ->
    case parse(Bin) of
        { ok, URI } ->
            URI;
        { error, _ } = Error ->
            error(Error)
    end;
make(Parts) ->
    Init = #uri{ host = { ipv4, { 0, 0, 0, 0 } } },
    lists:foldl(fun(Option, URI) ->
                       set_part(Option, URI)
                end,
                Init,
                Parts).

%% @doc Make URI comparable with =:= erlang operator.  This means that
%% if make_key(UriA) =:= make_key(UriB) then they equal by RFC3261 19.1.4 URI Comparison.
-spec make_key(uri()) -> uri().
make_key(#uri{} = URI) ->
    #uri{
       %% A SIP and SIPS URI are never equivalent.
       scheme = URI#uri.scheme,
       %%
       %% Comparison of the userinfo of SIP and SIPS URIs is case-
       %% sensitive.  This includes userinfo containing passwords or
       %% formatted as telephone-subscribers.  Comparison of all other
       %% components of the URI is case-insensitive unless explicitly
       %% defined otherwise.
       %%
       %% For two URIs to be equal, the user, password, host, and port
       %% components must match.
       user  = userinfo_key(URI#uri.user),
       host  = ersip_host:make_key(URI#uri.host),
       port  = URI#uri.port,
       params = params_key(URI#uri.params),
       headers = headers_key(URI#uri.headers)
      }.


%% @doc Parse URI from the binary
%% SIP-URI          =  "sip:" [ userinfo ] hostport
%%                     uri-parameters [ headers ]
%% SIPS-URI         =  "sips:" [ userinfo ] hostport
%%                     uri-parameters [ headers ]
-spec parse(binary()) -> { ok, uri() } | { error, { einval, atom() } }.
parse(Binary) ->
    case split_scheme(Binary) of
        { <<>>, _ } ->
            { error, { einval, invalid_scheme } };
        { S, R } ->
            case ersip_bin:to_lower(S) of
                <<"sip">> ->
                    parse_usesrinfo({ scheme, sip }, R);
                <<"sips">> ->
                    parse_usesrinfo({ scheme, sips }, R);
                Scheme ->
                    case check_token(Scheme) of
                        true ->
                            parse_usesrinfo({ scheme, S }, R);
                        false ->
                            { error, { invalid_scheme, S } }
                    end
            end
    end.

-spec assemble(uri()) -> iolist().
assemble(#uri{} = URI) ->
    [ assemble_scheme(URI#uri.scheme), $:,
      case URI#uri.user of
          undefined ->
              [];
          User ->
              [ assemble_user(User), $@ ]
      end,
      ersip_host:assemble(URI#uri.host),
      case URI#uri.port of
          undefined ->
              [];
          Port ->
              [ $:, integer_to_binary(Port) ]
      end,
      assemble_params(URI#uri.params)
    ].

-spec params(uri()) -> uri_params().
params(#uri{ params = Params }) ->
    Params.

%% @doc set paramter of the URI
-spec set_param(uri_param_name(), term(), uri()) -> uri().
set_param(ParamName, Value, #uri{ params = P } = URI) ->
    URI#uri{ params = P#{ ParamName => Value } }.

%% @doc set paramter of the URI
-spec set_part(uri_part(), uri()) -> uri().
set_part({ scheme, _ } = Scheme, #uri{} = URI) -> URI#uri{ scheme = Scheme };
set_part({ user, U } = User,     #uri{} = URI) when is_binary(U) -> URI#uri{ user = User };
set_part({ port, P },            #uri{} = URI) when is_integer(P) -> URI#uri{ port = P };
set_part({ host, H },            #uri{} = URI) ->
    case ersip_host:is_host(H) of
        true ->
            URI#uri{ host = H };
        false ->
            error(badarg)
    end;
set_part(_, _) ->
    error(badarg).

%%                                                       dialog
%%                                           reg./redir. Contact/
%%               default  Req.-URI  To  From  Contact   R-R/Route  external
%% user          --          o      o    o       o          o         o
%% password      --          o      o    o       o          o         o
%% host          --          m      m    m       m          m         m
%% port          (1)         o      -    -       o          o         o
%% user-param    ip          o      o    o       o          o         o
%% method        INVITE      -      -    -       -          -         o
%% maddr-param   --          o      -    -       o          o         o
%% ttl-param     1           o      -    -       o          -         o
%% transp.-param (2)         o      -    -       o          o         o
%% lr-param      --          o      -    -       -          o         o
%% other-param   --          o      o    o       o          o         o
%% headers       --          -      -    -       o          -         o
-spec clear_not_allowed_parts(ruri, uri()) -> uri().
clear_not_allowed_parts(ruri, #uri{ params = P } = URI) ->
    URI#uri{ params = maps:without([ method ], P),
             headers = #{}
           };
clear_not_allowed_parts(record_route, #uri{ params = P } = URI) ->
    URI#uri{ params = maps:without([ method, ttl ], P),
             headers = #{}
           }.

%%%===================================================================
%%% Internal implementation
%%%===================================================================

-spec parse_usesrinfo(Scheme, binary()) -> { ok, uri() } | { error, { einval, atom() } } when
      Scheme :: scheme().
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
      Scheme :: scheme(),
      User   :: {user, binary() } | undefined.
parse_hostport(Scheme, User, R) ->
    { HostPort, Params, Headers } = split_uri(R),
    URI =
        case split_hostport(HostPort) of
            { ok, { HostBin, <<>> } } ->
                case ersip_host:parse(HostBin) of
                    { ok, Host } ->
                        { ok,
                          #uri{ scheme = Scheme,
                                user   = User,
                                host   = Host
                              }
                        };
                    _ ->
                        { error, { einval, host } }
                end;
            { ok, { HostBin, PortBin } } ->
                case { ersip_host:parse(HostBin), parse_port(PortBin) } of
                    { { ok, Host }, { ok, Port } } ->
                        { ok,
                          #uri{ scheme = Scheme,
                                user   = User,
                                host   = Host,
                                port   = Port
                              }
                        };
                    _ ->
                        { error, { einval, hostport } }
                end;
            { error, _ } = Error ->
                Error
        end,
    URI1 = maybe_add_params(URI, Params),
    maybe_add_headers(URI1, Headers).

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
      UriOrError :: { ok, uri() }
                  | { error, { einval, atom() } }.
maybe_add_params({error, _ } = Err, _) ->
    Err;
maybe_add_params({ok, #uri{} = URI}, <<>>) ->
    { ok, URI };
maybe_add_params({ok, #uri{} = URI}, ParamsBin) ->
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

-spec maybe_add_headers(UriOrError, binary()) -> { ok, uri() } | { error, { einval, atom() } } when
      UriOrError :: uri()
                  | { error, { einval, atom() } }.
maybe_add_headers({ error, _ } = Err, _) ->
    Err;
maybe_add_headers({ ok, #uri{} = URI }, <<>>) ->
    { ok, URI };
maybe_add_headers({ ok, #uri{} = URI }, Headers) ->
    { ok, HeadersList, <<>> } = ersip_parser_aux:parse_kvps(fun uri_header_validator/2, <<"&">>, Headers),
    { ok, URI#uri{ headers = maps:from_list(HeadersList) } }.

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
            case ersip_transport:parse(V) of
                { error, _ } = Err ->
                    Err;
                { ok, T } ->
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

%% @private
%% @doc URI userinfo part key
-spec userinfo_key(undefined | { user, binary() }) ->
                          undefined | { user, binary() }.
userinfo_key(undefined) ->
    undefined;
userinfo_key({user, Bin}) ->
    { user, ersip_bin:unquote_rfc_2396(Bin) }.

%% @private
%% @doc URI params key
-spec params_key(uri_params()) -> uri_params().
params_key(Params) ->
    maps:with([ user, transport, ttl, method ], Params).

%% @doc URI headers key
-spec headers_key(uri_headers()) -> uri_headers().
headers_key(Headers) ->
    maps:map(fun(Key, Value) ->
                     { Key,
                       ersip_bin:unquote_rfc_2396(Value)
                     }
             end,
             Headers).

-spec split_scheme(binary()) -> { binary(), binary() }.
split_scheme(Bin) ->
    case binary:split(Bin, <<":">>) of
        [ Scheme, Suffix ] ->
            { Scheme, Suffix };
        [ Suffix ] ->
            { <<>>, Suffix }
    end.

-spec split_uri(binary()) -> { HostPort, Params, Headers } when
      HostPort :: binary(),
      Params   :: binary(),
      Headers  :: binary().
split_uri(Bin) ->
    case binary:match(Bin, <<";">>) of
        nomatch ->
            { HostPort, Headers } = split_headers(Bin),
            { HostPort, <<>>, Headers };
        { _, 1 } ->
            { HostPort, Rest } = split_params(Bin),
            { Params, Headers } = split_headers(Rest),
            { HostPort, Params, Headers }
    end.

-spec split_hostport(binary()) -> Result when
      Result :: { ok, { binary(), binary() } }
              | { error, Error },
      Error :: { einval, invalid_ipv6_reference }
             | { invalid_port, binary() }.
split_hostport(<<$[, _/binary>> = IPv6RefPort) ->
    case binary:match(IPv6RefPort, <<"]">>) of
        nomatch ->
            { error, { einval, invalid_ipv6_reference } };
        { Pos, 1 } when Pos + 1 =:= byte_size(IPv6RefPort) ->
            %% No port specified
            { ok, { IPv6RefPort, <<>> } };
        { Pos, 1 } ->
            Host = binary:part(IPv6RefPort, { 0, Pos+1 }),
            Rest = binary:part(IPv6RefPort, { Pos+1, byte_size(IPv6RefPort)-Pos-1}),
            case Rest of
                <<$:, Port/binary>> when Port =/= <<>> ->
                    { ok, { Host, Port } };
                Else ->
                    { error, { invalid_port, Else } }
            end
    end;
split_hostport(IPOrHost) ->
    case binary:split(IPOrHost, <<":">>) of
        [ H, P ] ->
            { ok, { H, P } };
        [ H ] ->
            { ok, { H, <<>> } }
    end.

-spec split_headers(binary()) -> { binary(), binary() }.
split_headers(Bin) ->
    case binary:split(Bin, <<"?">>) of
        [ Prefix, Headers ] ->
            { Prefix, Headers };
        [ Prefix ] ->
            { Prefix, <<>> }
    end.


-spec split_params(binary()) -> { binary(), binary() }.
split_params(Bin) ->
    case binary:split(Bin, <<";">>) of
        [ Prefix, Headers ] ->
            { Prefix, Headers }
    end.

-spec uri_header_validator(binary(), binary()) -> { ok, { binary(), binary() } }.
uri_header_validator(Key, Value) ->
    { ok, { ersip_bin:to_lower(ersip_bin:unquote_rfc_2396(Key)), Value } }.


assemble_scheme({ scheme, sip }) ->
    <<"sip">>;
assemble_scheme({ scheme, sips }) ->
    <<"sips">>;
assemble_scheme({ scheme, Scheme }) ->
    Scheme.

-spec assemble_user({ user, binary() }) -> binary().
assemble_user({ user, UserBin }) ->
    UserBin.

-spec assemble_params(uri_params()) -> [ iolist() ].
assemble_params(Params) ->
    lists:map(fun assemble_param/1,
              maps:to_list(Params)).

-spec assemble_param({ Name, Value }) -> iolist() when
      Name :: uri_param_name(),
      Value :: term().
assemble_param({ transport, Value }) ->
    [ <<";transport=">>, ersip_transport:assemble(Value) ];
assemble_param({ maddr, Host }) ->
    [ <<";maddr=">>, ersip_host:assemble(Host) ];
assemble_param({ lr, _ }) ->
    <<";lr">>;
assemble_param({ user, ip }) ->
    <<";user=ip">>;
assemble_param({ user, phone }) ->
    <<";user=phone">>;
assemble_param({ user, Bin }) when is_binary(Bin) ->
    [ <<";user=">>, Bin ];
assemble_param({ ttl, TTL }) ->
    [ <<";ttl=">>, integer_to_binary(TTL) ];
assemble_param({ Name, <<>> }) when is_binary(Name) ->
    <<";", Name/binary >>;
assemble_param({ Name, Value }) ->
    <<";", Name/binary, "=", Value/binary>>.
