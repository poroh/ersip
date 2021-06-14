%%%
%%% Copyright (c) 2021 Dmitry Poroh
%%% All rights reserved.
%%% Distributed under the terms of the MIT License. See the LICENSE file.
%%%
%%% SIP URI
%%%

-module(ersip_uri_sip).

-export([new/0,
         new/1,
         new_parsed/3,

         scheme/1,

         secure/1,
         set_secure/2,

         user/1,
         set_user/2,

         host/1,
         host_bin/1,
         set_host/2,

         port/1,
         set_port/2,

         transport/1,
         set_transport/2,
         clear_transport/1,

         loose_router/1,
         set_loose_router/2,

         maddr/1,
         set_maddr/2,
         clear_maddr/1,

         user_param/1,
         set_user_param/2,
         clear_user_param/1,

         ttl/1,
         set_ttl/2,
         clear_ttl/1,

         gen_param/2,
         set_gen_param/3,
         clear_gen_param/2,

         set_param/3,
         rebuild_header_values/1,
         params/1,

         raw_params/1,
         clear_all_params/1,

         raw_headers/1,
         set_raw_headers/2,

         clear_not_allowed_parts/2,

         make/1,
         parse/1,
         make_key/1,
         assemble/1,
         enrich_raw/2
        ]).

-export_type([sip_uri/0,
              user_param/0,
              ttl_param/0,
              uri_param_name/0,
              uri_params/0
             ]).

-include("ersip_sip_abnf.hrl").

%%===================================================================
%% Types
%%===================================================================

-record(sip_uri,
        {
         secure :: boolean(),
         %% user: The identifier of a particular resource at the host being
         %%    addressed.  The term "host" in this context frequently refers
         %%    to a domain.  The "userinfo" of a URI consists of this user
         %%    field, the password field, and the @ sign following them.  The
         %%    userinfo part of a URI is optional and MAY be absent when the
         %%    destination host does not have a notion of users or when the
         %%    host itself is the resource being identified.  If the @ sign is
         %%    present in a SIP or SIPS URI, the user field MUST NOT be empty.
         user   = undefined :: undefined
                             | {user, binary()},
         %% host: The host providing the SIP resource.  The host part contains
         %%    either a fully-qualified domain name or numeric IPv4 or IPv6
         %%    address.  Using the fully-qualified domain name form is
         %%    RECOMMENDED whenever possible.
         host               :: ersip_host:host(),
         %% original (binary) representation of host
         host_orig          :: binary() | undefined,
         %% port: The port number where the request is to be sent.
         port               :: undefined | inet:port_number(),
         %% URI parameters: Parameters affecting a request constructed from
         %% the URI.
         params = #{}       :: uri_params(),
         %% URI headers
         headers = #{}      :: uri_headers()
        }).

-type sip_uri() :: #sip_uri{}.

-type uri_params() :: #{transport => ersip_transport:transport(),
                        maddr     => ersip_host:host(),
                        ttl       => 0..255,
                        user      => phone | ip | binary(),
                        method    => binary(),
                        lr        => true
                       }.
-type uri_headers() ::  #{binary() => binary()}.

-type known_param() :: transport | lr | ttl | user | maddr.
-type known_param_value() :: ersip_transport:transport()
                           | boolean()
                           | ttl_param()
                           | user_param()
                           | ersip_host:host().

-type user_param() :: phone | ip | binary().
-type ttl_param() :: 0..255.
-type uri_param_name() :: known_param() | binary().

%%===================================================================
%% API
%%===================================================================
-define(IS_VALID_PORT(X), is_integer(P), P >= 0, P =< 65535; P == undefined).

%% @private
-spec new() -> sip_uri().
new() ->
    #sip_uri{secure = false, host = {ipv4, {0, 0, 0, 0}}}.

-spec new(ersip_host:host()) -> sip_uri().
new(Host) ->
    true = ersip_host:is_host(Host),
    #sip_uri{secure = false, host = Host}.

-spec new_parsed(binary() | undefined, ersip_host:host(), binary()) -> sip_uri().
new_parsed(undefined, Host, HostBin) ->
    #sip_uri{secure = false, host = Host, host_orig = HostBin};
new_parsed(User, Host, HostBin) ->
    #sip_uri{secure = false, user = {user, User}, host = Host, host_orig = HostBin}.


-spec scheme(sip_uri()) -> ersip_uri:scheme().
scheme(#sip_uri{secure = false}) ->
    {scheme, sip};
scheme(#sip_uri{secure = true}) ->
    {scheme, sips}.

-spec secure(sip_uri()) -> boolean().
secure(#sip_uri{secure = S}) ->
    S.

-spec set_secure(boolean(), sip_uri()) -> sip_uri().
set_secure(Secure, #sip_uri{} = U) when is_boolean(Secure) ->
    U#sip_uri{secure = Secure}.

%% @private
-spec user(sip_uri()) -> binary() | undefined.
user(#sip_uri{user = undefined}) ->
    undefined;
user(#sip_uri{user = {user, U}}) ->
    U.

%% @private
-spec set_user(binary(), sip_uri()) -> sip_uri().
set_user(NewUser, #sip_uri{} = D) when is_binary(NewUser) ->
    D#sip_uri{user = {user, NewUser}}.

%% @private
-spec host(sip_uri()) -> ersip_host:host().
host(#sip_uri{host = H}) ->
    H.

%% @private
-spec host_bin(sip_uri()) -> binary().
host_bin(#sip_uri{host_orig = undefined, host = H}) ->
    ersip_host:assemble_bin(H);
host_bin(#sip_uri{host_orig = HostBin}) when is_binary(HostBin) ->
    HostBin.

%% @private
-spec set_host(ersip_host:host(), sip_uri()) -> sip_uri().
set_host(H, #sip_uri{host = H} = URI) ->
    URI;
set_host(H, #sip_uri{} = URI) ->
    URI#sip_uri{host = H, host_orig = undefined}.

%% @private
-spec port(sip_uri()) -> undefined | inet:port_number().
port(#sip_uri{port = P}) ->
    P.

%% @private
-spec set_port(undefined | inet:port_number(), sip_uri()) -> sip_uri().
set_port(P, #sip_uri{}= U) when ?IS_VALID_PORT(P) ->
    U#sip_uri{port = P}.

%% @private
-spec transport(sip_uri()) -> undefined | ersip_transport:transport().
transport(#sip_uri{params = #{transport := T}}) ->
    T;
transport(#sip_uri{params = #{}}) ->
    undefined.

%% @private
-spec set_transport(ersip_transport:transport(), sip_uri()) -> sip_uri().
set_transport(Transport, #sip_uri{} = URI) ->
    set_sip_param(transport, Transport, URI).

%% @private
-spec clear_transport(sip_uri()) -> sip_uri().
clear_transport(#sip_uri{} = URI) ->
    clear_sip_param(transport, URI).

%% @private
-spec loose_router(sip_uri()) -> boolean().
loose_router(#sip_uri{params = #{lr := true}}) ->
    true;
loose_router(#sip_uri{}) ->
    false.

%% @private
-spec set_loose_router(boolean(), sip_uri()) -> sip_uri().
set_loose_router(true, #sip_uri{} = URI) ->
    set_sip_param(lr, true, URI);
set_loose_router(false, #sip_uri{} = URI) ->
    clear_sip_param(lr, URI).

%% @private
-spec maddr(sip_uri()) -> ersip_host:host() | undefined.
maddr(#sip_uri{params = #{maddr := Maddr}}) ->
    Maddr;
maddr(#sip_uri{}) ->
    undefined.

%% @private
-spec set_maddr(ersip_host:host(), sip_uri()) -> sip_uri().
set_maddr(Host, #sip_uri{} = URI) ->
    case ersip_host:is_host(Host) of
        true ->
            set_sip_param(maddr, Host, URI);
        false ->
            error({host_expected, Host})
    end.

%% @private
-spec clear_maddr(sip_uri()) -> sip_uri().
clear_maddr(#sip_uri{} = URI) ->
    clear_sip_param(maddr, URI).

%% @private
-spec user_param(sip_uri()) -> user_param() | undefined.
user_param(#sip_uri{params = #{user := U}}) ->
    U;
user_param(#sip_uri{}) ->
    undefined.

%% @private
-spec set_user_param(user_param(), sip_uri()) -> sip_uri().
set_user_param(UserParam, #sip_uri{} = URI)
  when UserParam == ip; UserParam == phone; is_binary(UserParam) ->
    set_sip_param(user, UserParam, URI).

%% @private
-spec clear_user_param(sip_uri()) -> sip_uri().
clear_user_param(#sip_uri{} = URI) ->
    clear_sip_param(user, URI).

%% @private
-spec ttl(sip_uri()) -> ttl_param() | undefined.
ttl(#sip_uri{params = #{ttl := TTL}}) ->
    TTL;
ttl(#sip_uri{}) ->
    undefined.

%% @private
-spec set_ttl(ttl_param(), sip_uri()) -> sip_uri().
set_ttl(TTL, #sip_uri{} = URI) when TTL >= 0, TTL =< 255 ->
    set_sip_param(ttl, TTL, URI);
set_ttl(TTL, #sip_uri{}) ->
    error({ttl_expected, TTL}).

%% @private
-spec clear_ttl(sip_uri()) -> sip_uri().
clear_ttl(#sip_uri{} = URI) ->
    clear_sip_param(ttl, URI).

%% @private
-spec gen_param(binary(), sip_uri()) -> binary() | undefined.
gen_param(Name, #sip_uri{params = P}) when is_binary(Name) ->
    case find_known_param(Name) of
        {ok, KnownParam} ->
            case P of
                #{KnownParam := V} ->
                    case raw_param({KnownParam, V}) of
                        {_, BinV} -> BinV;
                        _ -> true
                    end;
                _ -> undefined
            end;
        error ->
            maps:get(ersip_bin:to_lower(Name), P, undefined)
    end.


%% @private
-spec set_gen_param(binary(), binary(), sip_uri()) -> sip_uri().
set_gen_param(Name, Value, #sip_uri{} = U)
  when is_binary(Name) andalso (is_binary(Value) orelse Value == true) ->
    ParamBin =
        case Value of
            true -> Name;
            _ -> <<Name/binary, "=", Value/binary>>
        end,
    case ersip_uri_sip_parser:parse_and_add_param(ParamBin, U) of
        {ok, #sip_uri{} = NewU} -> NewU;
        {error, Reason} ->
            error({invalid_value, Reason})
    end.

%% @private
-spec clear_gen_param(binary(), sip_uri()) -> sip_uri().
clear_gen_param(Name, #sip_uri{params = P} = U) when is_binary(Name) ->
    case find_known_param(Name) of
        {ok, KnownParam} ->
            U#sip_uri{params = maps:remove(KnownParam, P)};
        error ->
            U#sip_uri{params = maps:remove(ersip_bin:to_lower(Name), P)}
    end.


%% @private
-spec set_param(uri_param_name(), term(), sip_uri()) -> sip_uri().
set_param(ParamName, Value, #sip_uri{params = P} = U) ->
    U#sip_uri{params = P#{ParamName => Value}}.

%% @private.
-spec rebuild_header_values(sip_uri()) -> sip_uri().
rebuild_header_values(#sip_uri{headers = H} = U) ->
    NewH = maps:map(fun(_, V) -> rebuild_header_value(V) end, H),
    U#sip_uri{headers = NewH}.

%% @private
-spec params(sip_uri()) -> uri_params().
params(#sip_uri{params = Params}) ->
    Params.

%% @doc Get raw URI params as list.
-spec raw_params(sip_uri()) -> [{binary(), binary()} | binary()].
raw_params(#sip_uri{params = Params}) ->
    lists:map(fun raw_param/1, maps:to_list(Params)).

%% @doc Get raw URI headers as list.
-spec raw_headers(sip_uri()) -> [{binary(), binary()}].
raw_headers(#sip_uri{headers = Headers}) ->
    maps:to_list(Headers).

%% @doc Set raw URI headers from list.
-spec set_raw_headers([{binary(), binary()}], sip_uri()) -> sip_uri().
set_raw_headers(Headers, #sip_uri{} = Data) ->
    HMap = maps:from_list(Headers),
    Data#sip_uri{headers = HMap}.

-spec clear_all_params(sip_uri()) -> sip_uri().
clear_all_params(#sip_uri{} = U) ->
    U#sip_uri{params = #{}}.

%% @doc Clear not allowed par of the URI in context.
%% ```
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
%% '''
-spec clear_not_allowed_parts(ruri | record_route, sip_uri()) -> sip_uri().
clear_not_allowed_parts(ruri, #sip_uri{params = P} = URI) ->
    URI#sip_uri{params = maps:without([<<"method">>], P),
                headers = #{}
               };
clear_not_allowed_parts(record_route, #sip_uri{params = P} = URI) ->
    URI#sip_uri{params = maps:without([<<"method">>, ttl], P),
                headers = #{}
               }.

-spec parse(binary()) -> ersip_uri_sip_parser:parse_result().
parse(Bin) ->
    ersip_uri_sip_parser:parse(Bin).

-spec make(binary()) -> sip_uri().
make(Bin) ->
    case parse(Bin) of
        {ok, SIPURI} ->
            SIPURI;
        {error, Reason} ->
            error({invalid_sip_uri, Reason})
    end.

-spec make_key(sip_uri()) -> sip_uri().
make_key(#sip_uri{} = URIData) ->
    %%
    %% Comparison of the userinfo of SIP and SIPS URIs is case-
    %% sensitive.  This includes userinfo containing passwords or
    %% formatted as telephone-subscribers.  Comparison of all other
    %% components of the URI is case-insensitive unless explicitly
    %% defined otherwise.
    %%
    %% For two URIs to be equal, the user, password, host, and port
    %% components must match.
    #sip_uri{secure = URIData#sip_uri.secure,
             user  = userinfo_key(URIData#sip_uri.user),
             host  = ersip_host:make_key(URIData#sip_uri.host),
             host_orig = undefined,
             port  = URIData#sip_uri.port,
             params = params_key(URIData#sip_uri.params),
             headers = headers_key(URIData#sip_uri.headers)
            }.

-spec assemble(sip_uri()) -> iolist().
assemble(#sip_uri{} = SIPData) ->
    [case SIPData#sip_uri.secure of
         true -> <<"sips:">>;
         false -> <<"sip:">>
     end,
     case SIPData#sip_uri.user of
         undefined ->
             [];
         User ->
             [assemble_user(User), $@]
     end,
     case SIPData#sip_uri.host_orig of
         undefined ->
             ersip_host:assemble(SIPData#sip_uri.host);
         Val when is_binary(Val) ->
             Val
     end,
     case SIPData#sip_uri.port of
         undefined ->
             [];
         Port ->
             [$:, integer_to_binary(Port)]
     end,
     assemble_params(SIPData#sip_uri.params),
     assemble_headers(SIPData#sip_uri.headers)
    ].

-spec enrich_raw(sip_uri(), ersip_uri:raw()) -> ersip_uri:raw().
enrich_raw(#sip_uri{} = URI, Base) ->
    RawList =
      [case P of
           {K, V} -> {ersip_bin:to_lower(K), V};
           K -> {ersip_bin:to_lower(K), <<>>}
       end || P <- raw_params(URI)],
    Parts = [{user, user(URI)},
             {host, ersip_host:raw(host(URI))},
             {port, port(URI)},
             {params, maps:from_list(RawList)},
             {headers, raw_headers(URI)}],
    NonEmpty = [{K, V} || {K, V} <- Parts, V /= undefined andalso V /= [] andalso V /= #{}],
    Base#{sip => maps:from_list(NonEmpty)}.




%%===================================================================
%% Implementation
%%===================================================================

%% @private
-spec set_sip_param(known_param(), known_param_value(), sip_uri()) -> sip_uri().
set_sip_param(Name, Value, #sip_uri{params = P} = URI) ->
    URI#sip_uri{params = P#{Name => Value}}.

%% @private
-spec clear_sip_param(known_param(), sip_uri()) -> sip_uri().
clear_sip_param(Name, #sip_uri{params = P} = URI) ->
    URI#sip_uri{params = maps:remove(Name, P)}.

%% @private
-spec find_known_param(binary()) -> {ok, known_param()} | error.
find_known_param(<<"ttl">>)       -> {ok, ttl};
find_known_param(<<"maddr">>)     -> {ok, maddr};
find_known_param(<<"user">>)      -> {ok, user};
find_known_param(<<"lr">>)        -> {ok, lr};
find_known_param(<<"transport">>) -> {ok, transport};
find_known_param(_)               -> error.

%% @private
-spec raw_param({uri_param_name(), term()}) -> {binary(), binary()} | binary().
raw_param({transport, Value}) -> {<<"transport">>, ersip_transport:assemble_bin(Value)};
raw_param({maddr, Host})      -> {<<"maddr">>,    ersip_host:assemble_bin(Host)};
raw_param({lr, _})            -> <<"lr">>;
raw_param({user, ip})         -> {<<"user">>, <<"ip">>};
raw_param({user, phone})      -> {<<"user">>, <<"phone">>};
raw_param({user, Bin})
  when is_binary(Bin)         -> {<<"user">>, Bin};
raw_param({ttl, TTL})         -> {<<"ttl">>, integer_to_binary(TTL)};
raw_param({Name, <<>>})
  when is_binary(Name)        -> Name;
raw_param({Name, Value})      -> {Name, Value}.


-spec rebuild_header_value(binary()) -> binary().
rebuild_header_value(Value) ->
    Bytes = binary_to_list(ersip_uri_parser_aux:unquote_hex(Value)),
    Escaped = [escape_header_byte(B) || B <- Bytes],
    iolist_to_binary(Escaped).


%% hvalue          =  *( hnv-unreserved / unreserved / escaped )
%% hnv-unreserved  =  "[" / "]" / "/" / "?" / ":" / "+" / "$"
-spec escape_header_byte(char()) -> char() | string().
escape_header_byte(V) when ?is_unreserved(V);
                           ?is_hnv_unreserved(V) ->
    V;
escape_header_byte(V) ->
    io_lib:format("%~2.16.0B", [V]).


-spec assemble_user({user, binary()}) -> binary().
assemble_user({user, UserBin}) ->
    UserBin.

-spec assemble_params(uri_params()) -> [iolist()].
assemble_params(Params) ->
    lists:map(fun assemble_param/1,
              maps:to_list(Params)).

-spec assemble_headers(uri_headers()) -> [iolist()].
assemble_headers(Headers) ->
    case Headers == #{} of
        true ->
            [];
        false ->
            [$?,
             ersip_iolist:join(
               <<"&">>,
               lists:map(fun ({Name, Value}) ->
                                 [Name, $=, Value]
                         end,
                         maps:to_list(Headers)))
            ]
    end.


-spec assemble_param({Name, Value}) -> iolist() when
      Name :: uri_param_name(),
      Value :: term().
assemble_param(Pair) ->
    case raw_param(Pair) of
        {Name, Val} -> [<<";">>, Name, <<"=">>, Val];
        Name -> [<<";">>, Name]
    end.

%% @private
%% @doc URI userinfo part key
-spec userinfo_key(undefined | {user, binary()}) -> undefined | {user, binary()}.
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
                     {Key, ersip_bin:unquote_rfc_2396(Value)}
             end,
             Headers).
