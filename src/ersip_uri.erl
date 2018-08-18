%%
%% Copyright (c) 2017, 2018 Dmitry Poroh
%% All rights reserved.
%% Distributed under the terms of the MIT License. See the LICENSE file.
%%
%% SIP URI
%%

-module(ersip_uri).

-export([scheme/1,
         get/2,
         make/1,
         make_key/1,
         parse/1,
         assemble/1,
         params/1,
         clear_params/1,
         set_param/3,
         clear_not_allowed_parts/2,
         assemble_scheme/1
        ]).
-export_type([uri/0, scheme/0]).


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

-type uri_part_name() :: scheme
                       | user
                       | host
                       | port.
-type uri_part() :: scheme()
                  | {user, binary()}
                  | {host, ersip_host:host()}
                  | {port, 0..65535}.

-type scheme() :: uri_scheme().

%%%===================================================================
%%% API
%%%===================================================================

-spec scheme(uri()) -> scheme().
scheme(#uri{scheme = S}) ->
    S.

-spec get(Parts, uri()) -> Result when
      Parts :: uri_part_name()
             | [uri_part_name()],
      Result :: uri_part()
              | [uri_part()].
get(Part, URI) when is_atom(Part) ->
    get_part(Part, URI);
get(Parts, URI) when is_list(Parts) ->
    lists:map(fun(Part) ->
                      get_part(Part, URI)
              end,
              Parts).

-spec make(PartsOrBin :: [uri_part()] | binary()) -> uri().
make(Bin) when is_binary(Bin) ->
    case parse(Bin) of
        {ok, URI} ->
            URI;
        {error, _} = Error ->
            error(Error)
    end;
make(Parts) ->
    Init = #uri{data = #sip_uri_data{host = {ipv4, {0, 0, 0, 0}}}},
    lists:foldl(fun(Option, URI) ->
                        set_part(Option, URI)
                end,
                Init,
                Parts).

%% @doc Make URI comparable with =:= erlang operator.  This means that
%% if make_key(UriA) =:= make_key(UriB) then they equal by RFC3261 19.1.4 URI Comparison.
-spec make_key(uri()) -> uri().
make_key(#uri{} = URI) ->
    #uri{scheme = URI#uri.scheme,
         data = make_data_key(URI#uri.scheme, URI#uri.data)
        }.

%% @doc Parse URI from the binary
%% SIP-URI          =  "sip:" [userinfo] hostport
%%                     uri-parameters [headers]
%% SIPS-URI         =  "sips:" [userinfo] hostport
%%                     uri-parameters [headers]
-spec parse(binary()) -> {ok, uri()} | {error, {einval, atom()}}.
parse(Binary) ->
    case split_scheme(Binary) of
        {<<>>, _} ->
            {error, {einval, invalid_scheme}};
        {S, R} ->
            parse_uri(ersip_bin:to_lower(S), S, R)
    end.

-spec assemble(uri()) -> iolist().
assemble(#uri{scheme = Scheme, data = Data}) ->
    [assemble_scheme(Scheme), $:,
     assemble_data(Data)
    ].

-spec params(uri()) -> uri_params().
params(#uri{data = #sip_uri_data{params = Params}}) ->
    Params.

-spec clear_params(uri()) -> uri().
clear_params(#uri{data = #sip_uri_data{} = SIPData} = URI) ->
    URI#uri{data = SIPData#sip_uri_data{params = #{}}};
clear_params(#uri{} = URI) ->
    URI.

%% @doc set paramter of the URI
-spec set_param(uri_param_name(), term(), uri() | sip_uri_data()) -> uri() | sip_uri_data().
set_param(ParamName, Value, #uri{data = SIPData} = URI) ->
    URI#uri{data = set_param(ParamName, Value, SIPData)};
set_param(ParamName, Value, #sip_uri_data{params = P} = SIPData) ->
    SIPData#sip_uri_data{params = P#{ParamName => Value}}.

%% @doc set paramter of the URI
-spec set_part(uri_part(), uri()) -> uri().
set_part({scheme, _} = Scheme, #uri{} = URI) ->
    URI#uri{scheme = Scheme};
set_part({user, U} = User, #uri{data = #sip_uri_data{} = SIPData} = URI) when is_binary(U) ->
    URI#uri{data = SIPData#sip_uri_data{user = User}};
set_part({port, P}, #uri{data = #sip_uri_data{} = SIPData} = URI) when is_integer(P) ->
    URI#uri{data = SIPData#sip_uri_data{port = P}};
set_part({host, H}, #uri{data = #sip_uri_data{} = SIPData} = URI) ->
    case ersip_host:is_host(H) of
        true ->
            URI#uri{data = SIPData#sip_uri_data{host = H}};
        false ->
            error({invalid_host, H})
    end;
set_part(Part, _) ->
    error({invalid_part, Part}).

-spec get_part(uri_part_name(), uri()) -> uri_part().
get_part(scheme, #uri{scheme = Scheme}) ->
    Scheme;
get_part(user, #uri{data = #sip_uri_data{user = User}}) ->
    User;
get_part(port, #uri{data = #sip_uri_data{port = Port}}) ->
    {port, Port};
get_part(host, #uri{data = #sip_uri_data{host = Host}}) ->
    {host, Host}.

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
-spec clear_not_allowed_parts(Type, uri()) -> uri() when
      Type :: ruri
            | record_route.
clear_not_allowed_parts(ruri, #uri{data = #sip_uri_data{params = P} = SIPData} = URI) ->
    URI#uri{data = SIPData#sip_uri_data{
                     params = maps:without([method], P),
                     headers = #{}
                    }
           };
clear_not_allowed_parts(ruri, URI) ->
    %% For schemes other than sip/sips we do not clear anything
    URI;
clear_not_allowed_parts(record_route, #uri{data = #sip_uri_data{params = P} = SIPData} = URI) ->
    URI#uri{data = SIPData#sip_uri_data{
                     params = maps:without([method, ttl], P),
                     headers = #{}
                    }};
clear_not_allowed_parts(record_route, URI) ->
    %% For schemes other than sip/sips we do not clear anything
    URI.

%%%===================================================================
%%% Internal implementation
%%%===================================================================

-spec parse_uri(LowerScheme, Scheme, Data) -> {ok, uri()} | {error, term()} when
      LowerScheme :: binary(),
      Scheme      :: binary(),
      Data        :: binary().
parse_uri(<<"sip">>, _, R) ->
    case parse_sipdata(R) of
        {error, _} = Error ->
            Error;
        {ok, SipData} ->
            {ok, #uri{scheme = {scheme, sip},
                      data   = SipData
                     }
            }
    end;
parse_uri(<<"sips">>, _, R) ->
    case parse_sipdata(R) of
        {error, _} = Error ->
            Error;
        {ok, SipData} ->
            {ok, #uri{scheme = {scheme, sips},
                      data   = SipData
                     }
            }
    end;
parse_uri(_, SchemeBin, R) ->
    case check_token(SchemeBin) of
        true ->
            {ok, #uri{scheme = {scheme, SchemeBin},
                      data   = #absolute_uri_data{opaque = R}
                     }
            };
        false ->
            {error, {invalid_scheme, SchemeBin}}
    end.

-spec parse_sipdata(binary()) -> {ok, sip_uri_data()} | {error, term()}.
parse_sipdata(Bin) ->
    parse_usesrinfo(Bin).

-spec parse_usesrinfo(binary()) -> {ok, sip_uri_data()} | {error, term()}.
parse_usesrinfo(Bin) ->
    case binary:split(Bin, <<"@">>) of
        [Userinfo, R] ->
            case check_userinfo(Userinfo) of
                true ->
                    parse_hostport({user, Userinfo}, R);
                _ ->
                    {error, {einval, userinfo}}
            end;
        [R] ->
            parse_hostport(undefined, R)
    end.

%% hostport         =  host [":" port]
-spec parse_hostport(User, binary()) -> {ok, sip_uri_data()} | {error, {einval, atom()}} when
      User   :: {user, binary()} | undefined.
parse_hostport(User, R) ->
    {HostPort, Params, Headers} = split_uri(R),
    MaybeSIPData =
        case split_hostport(HostPort) of
            {ok, {HostBin, <<>>}} ->
                case ersip_host:parse(HostBin) of
                    {ok, Host} ->
                        {ok, #sip_uri_data{user = User, host = Host }};
                    _ ->
                        {error, {einval, host}}
                end;
            {ok, {HostBin, PortBin}} ->
                case {ersip_host:parse(HostBin), parse_port(PortBin)} of
                    {{ok, Host}, {ok, Port}} ->
                        {ok, #sip_uri_data{user = User, host = Host, port = Port}};
                    _ ->
                        {error, {einval, hostport}}
                end;
            {error, _} = Error ->
                Error
        end,
    MaybeSIPData1 = maybe_add_params(MaybeSIPData, Params),
    maybe_add_headers(MaybeSIPData1, Headers).

%% port           =  1*DIGIT
-spec parse_port(binary()) -> {ok, 0..65535} |  {error, {einval, port}}.
parse_port(Bin) ->
    case catch binary_to_integer(Bin) of
        Int when is_integer(Int) andalso Int >= 0 andalso Int =< 65535 ->
            {ok, Int};
        _ ->
            {error, {einval, port}}
    end.

%% uri-parameters    =  *( ";" uri-parameter)
%% uri-parameter     =  transport-param / user-param / method-param
%%                      / ttl-param / maddr-param / lr-param / other-param
-spec maybe_add_params(SIPDataOrError, binary()) -> {ok, sip_uri_data()} | {error, {einval, atom()}} when
      SIPDataOrError :: {ok, sip_uri_data()}
                      | {error, {einval, atom()}}.
maybe_add_params({error, _} = Err, _) ->
    Err;
maybe_add_params({ok, #sip_uri_data{} = SIPData}, <<>>) ->
    {ok, SIPData};
maybe_add_params({ok, #sip_uri_data{} = SIPData}, ParamsBin) ->
    ParamsList = binary:split(ParamsBin, <<";">>),
    R =
        lists:foldl(fun(_, {error, _} = Err) ->
                            Err;
                       (Param, #sip_uri_data{} = SIPData1) ->
                            parse_and_add_param(Param, SIPData1)
                    end,
                    SIPData,
                    ParamsList),
    case R of
        #sip_uri_data{} ->
            {ok, R};
        {error, _} = Error ->
            Error
    end.

-spec maybe_add_headers(SIPDataOrError, binary()) -> {ok, sip_uri_data()} | {error, {einval, atom()}} when
      SIPDataOrError :: sip_uri_data()
                      | {error, {einval, atom()}}.
maybe_add_headers({error, _} = Err, _) ->
    Err;
maybe_add_headers({ok, #sip_uri_data{} = SIPData}, <<>>) ->
    {ok, SIPData};
maybe_add_headers({ok, #sip_uri_data{} = SIPData}, Headers) ->
    {ok, HeadersList, <<>>} = ersip_parser_aux:parse_kvps(fun uri_header_validator/2, <<"&">>, Headers),
    {ok, SIPData#sip_uri_data{headers = maps:from_list(HeadersList)}}.

%% @private
%% @doc Parse and add parameters described in RFC3261
-spec parse_and_add_param(binary(), sip_uri_data()) -> {ok, sip_uri_data()} | {error, {einval, atom()}}.
parse_and_add_param(Param, SIPData) ->
    Pair =
        case binary:split(Param, <<"=">>) of
            [Name] ->
                {ersip_bin:to_lower(Name), <<>>};
            [Name, Value] ->
                {ersip_bin:to_lower(Name), Value}
        end,

    case Pair of
        {<<"transport">>, V} ->
            %% transport-param   =  "transport="
            %%                      ( "udp" / "tcp" / "sctp" / "tls"
            %%                      / other-transport)
            %% other-transport   =  token
            %%
            case ersip_transport:parse(V) of
                {error, _} = Err ->
                    Err;
                {ok, T} ->
                    set_param(transport, T, SIPData)
            end;

        {<<"maddr">>, A} ->
            %% maddr-param       =  "maddr=" host
            case ersip_host:parse(A) of
                {ok, Host} ->
                    set_param(maddr, Host, SIPData);
                _ ->
                    {error, {einval, maddr}}
            end;

        {<<"user">>, U} ->
            %%  user-param        =  "user=" ( "phone" / "ip" / other-user)
            case ersip_bin:to_lower(U) of
                <<"phone">> ->
                    set_param(user, phone, SIPData);
                <<"ip">> ->
                    set_param(user, ip, SIPData);
                _ ->
                    case check_token(U) of
                        true ->
                            set_param(user, U, SIPData);
                        false ->
                            {error, {einval, user_param}}
                    end
            end;

        {<<"lr">>, _} ->
            set_param(lr, true, SIPData);

        {<<"ttl">>, TTLBin} ->
            case catch binary_to_integer(TTLBin) of
                TTL when is_integer(TTL) andalso TTL >= 0 andalso TTL =< 255 ->
                    set_param(ttl, TTL, SIPData);
                _ ->
                    {error, {einval, ttl}}
            end;

        {Other, OtherVal} ->
            %% TODO check Other & OtherVal for compliance.
            set_param(Other, OtherVal, SIPData)
    end.

%% userinfo         =  ( user / telephone-subscriber ) [":" password] "@"
%% password         =  *( unreserved / escaped / "&" / "=" / "+" / "$" / "," )
-spec check_userinfo(binary()) -> boolean().
check_userinfo(Bin) ->
    case binary:split(Bin, <<":">>) of
        [User, Password] ->
            check_user(User, start) andalso check_password(Password);
        [User] ->
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


-spec make_data_key(scheme(), uri_data()) -> sip_uri_data().
make_data_key({scheme, sip}, #sip_uri_data{} = SIPURIData) ->
    make_sip_data_key(SIPURIData);
make_data_key({scheme, sips}, #sip_uri_data{} = SIPURIData) ->
    make_sip_data_key(SIPURIData);
make_data_key(Scheme, _) ->
    error({cannot_make_key, {unsupported_scheme, Scheme}}).

-spec make_sip_data_key(sip_uri_data()) -> sip_uri_data().
make_sip_data_key(#sip_uri_data{} = URIData) ->
    %%
    %% Comparison of the userinfo of SIP and SIPS URIs is case-
    %% sensitive.  This includes userinfo containing passwords or
    %% formatted as telephone-subscribers.  Comparison of all other
    %% components of the URI is case-insensitive unless explicitly
    %% defined otherwise.
    %%
    %% For two URIs to be equal, the user, password, host, and port
    %% components must match.
    #sip_uri_data{user  = userinfo_key(URIData#sip_uri_data.user),
                  host  = ersip_host:make_key(URIData#sip_uri_data.host),
                  port  = URIData#sip_uri_data.port,
                  params = params_key(URIData#sip_uri_data.params),
                  headers = headers_key(URIData#sip_uri_data.headers)
                 }.

%% @private
%% @doc URI userinfo part key
-spec userinfo_key(undefined | {user, binary()}) ->
                          undefined | {user, binary()}.
userinfo_key(undefined) ->
    undefined;
userinfo_key({user, Bin}) ->
    {user, ersip_bin:unquote_rfc_2396(Bin)}.

%% @private
%% @doc URI params key
-spec params_key(uri_params()) -> uri_params().
params_key(Params) ->
    maps:with([user, transport, ttl, method], Params).

%% @doc URI headers key
-spec headers_key(uri_headers()) -> uri_headers().
headers_key(Headers) ->
    maps:map(fun(Key, Value) ->
                     {Key,
                      ersip_bin:unquote_rfc_2396(Value)
                     }
             end,
             Headers).

-spec split_scheme(binary()) -> {binary(), binary()}.
split_scheme(Bin) ->
    case binary:split(Bin, <<":">>) of
        [Scheme, Suffix] ->
            {Scheme, Suffix};
        [Suffix] ->
            {<<>>, Suffix}
    end.

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

-spec split_hostport(binary()) -> Result when
      Result :: {ok, {binary(), binary()}}
              | {error, Error},
      Error :: {einval, invalid_ipv6_reference}
             | {invalid_port, binary()}.
split_hostport(<<$[, _/binary>> = IPv6RefPort) ->
    case binary:match(IPv6RefPort, <<"]">>) of
        nomatch ->
            {error, {einval, invalid_ipv6_reference}};
        {Pos, 1} when Pos + 1 =:= byte_size(IPv6RefPort) ->
            %% No port specified
            {ok, {IPv6RefPort, <<>>}};
        {Pos, 1} ->
            Host = binary:part(IPv6RefPort, {0, Pos+1}),
            Rest = binary:part(IPv6RefPort, {Pos+1, byte_size(IPv6RefPort)-Pos-1}),
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

-spec uri_header_validator(binary(), binary()) -> {ok, {binary(), binary()}}.
uri_header_validator(Key, Value) ->
    {ok, {ersip_bin:to_lower(ersip_bin:unquote_rfc_2396(Key)), Value}}.


assemble_scheme({scheme, sip}) ->
    <<"sip">>;
assemble_scheme({scheme, sips}) ->
    <<"sips">>;
assemble_scheme({scheme, Scheme}) ->
    Scheme.


-spec assemble_data(uri_data()) -> iolist().
assemble_data(#sip_uri_data{} = SIPData) ->
    [case SIPData#sip_uri_data.user of
         undefined ->
             [];
         User ->
             [assemble_user(User), $@]
     end,
     ersip_host:assemble(SIPData#sip_uri_data.host),
     case SIPData#sip_uri_data.port of
         undefined ->
             [];
         Port ->
             [$:, integer_to_binary(Port)]
     end,
     assemble_params(SIPData#sip_uri_data.params)
    ];
assemble_data(#absolute_uri_data{opaque = Data}) ->
    Data.

-spec assemble_user({user, binary()}) -> binary().
assemble_user({user, UserBin}) ->
    UserBin.

-spec assemble_params(uri_params()) -> [iolist()].
assemble_params(Params) ->
    lists:map(fun assemble_param/1,
              maps:to_list(Params)).

-spec assemble_param({Name, Value}) -> iolist() when
      Name :: uri_param_name(),
      Value :: term().
assemble_param({transport, Value}) ->
    [<<";transport=">>, ersip_transport:assemble(Value)];
assemble_param({maddr, Host}) ->
    [<<";maddr=">>, ersip_host:assemble(Host)];
assemble_param({lr, _}) ->
    <<";lr">>;
assemble_param({user, ip}) ->
    <<";user=ip">>;
assemble_param({user, phone}) ->
    <<";user=phone">>;
assemble_param({user, Bin}) when is_binary(Bin) ->
    [<<";user=">>, Bin];
assemble_param({ttl, TTL}) ->
    [<<";ttl=">>, integer_to_binary(TTL)];
assemble_param({Name, <<>>}) when is_binary(Name) ->
    <<";", Name/binary >>;
assemble_param({Name, Value}) ->
    <<";", Name/binary, "=", Value/binary>>.

