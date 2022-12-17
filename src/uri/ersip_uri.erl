%%%
%%% Copyright (c) 2017, 2018, 2019, 2020, 2021 Dmitry Poroh
%%% All rights reserved.
%%% Distributed under the terms of the MIT License. See the LICENSE file.
%%%
%%% SIP URI
%%%

-module(ersip_uri).

-export([scheme/1,
         scheme_bin/1,
         data/1,

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

         get/2,
         make/1,
         make_key/1,
         parse/1,
         assemble/1,
         assemble_bin/1,
         params/1,
         raw_params/1,
         raw_headers/1,
         set_raw_headers/2,
         clear_params/1,
         set_param/3,
         clear_not_allowed_parts/2,
         rebuild_header_values/1,
         assemble_scheme/1,
         raw/1,

         is_sip/1,
         as_sip/1,
         from_sip/1,

         is_tel/1,
         as_tel/1,
         from_tel/1,

         set_part/2
        ]).
-export_type([uri/0, scheme/0, raw/0, make/0]).

%%===================================================================
%% Types
%%===================================================================

-record(uri, {type   = sip  :: type(),
              data          :: uri_data()
             }).

-type uri() :: #uri{}.
-type uri_data() :: ersip_uri_sip:sip_uri()
                  | ersip_uri_absolute:absolute_uri()
                  | ersip_uri_tel:tel_uri().

-type scheme()   :: {scheme, sip | sips | tel | binary()}.
-type type() :: sip | tel | absolute.

-type uri_part_name() :: scheme | user | host | port.
-type uri_part() :: scheme()  | {user, binary()} | {host, ersip_host:host()}
                  | {port, inet:port_number() | undefined}.

-type raw() :: #{scheme := binary(),
                 data := binary(),
                 sip => sip_uri_raw()}.

-type make() :: binary()
              | #{scheme := binary(),
                  data := binary(),
                  sip => sip_uri_raw()}.

-type sip_uri_raw() :: #{host := binary(),
                         user => binary(),
                         port => inet:port_number(),
                         params => #{binary() => binary()},
                         headers => key_value_list()
                        }.
-type key_value_list() :: [{binary(), binary()} | binary()].

-type parse_result() :: {ok, uri()} | {error, parse_error()}.
-type parse_error() :: {invalid_sip_uri, ersip_uri_sip_parser:parse_error()}
                     | {invalid_tel_uri, ersip_uri_tel:parse_error()}
                     | {invalid_abs_uri, ersip_uri_absolute:parse_error()}.

%%===================================================================
%% API
%%===================================================================
-define(IS_VALID_PORT(X), is_integer(P), P >= 0, P =< 65535; P == undefined).

%% @doc Scheme of the URI.
-spec scheme(uri()) -> scheme().
scheme(#uri{type = sip, data = D}) ->
    ersip_uri_sip:scheme(D);
scheme(#uri{type = tel}) ->
    {scheme, tel};
scheme(#uri{type = absolute, data = D}) ->
    ersip_uri_absolute:scheme(D).

%% @doc URI scheme in binary form.
-spec scheme_bin(uri()) -> binary().
scheme_bin(#uri{} = URI) ->
    case scheme(URI) of
        {scheme, sip}  -> <<"sip">>;
        {scheme, sips} -> <<"sips">>;
        {scheme, tel} -> <<"tel">>;
        {scheme, X} when is_binary(X) -> ersip_bin:to_lower(X)
    end.

%% @doc Get data of the URI (everything after scheme).
%% Example:
%% ```
%%    <<"+16505550505">> = ersip_uri:data(ersip_uri:make(<<"tel:+16505550505">>)).
%%    <<"a@b">> = ersip_uri:data(ersip_uri:make(<<"sip:a@b">>)).
%% '''
-spec data(uri()) -> binary().
data(#uri{type = T, data = Data}) ->
    {_, V} = ersip_uri_parser_aux:split_scheme(iolist_to_binary(assemble(T, Data))),
    V.

%% @doc Get user part of SIP URI.
%% Raises error if URI is not SIP(S) URI.
%% Example:
%% ```
%%    <<"alice">> = ersip_uri:user(ersip_uri:make(<<"sip:alice@atlanta.com">>)).
%%    <<"bob">> = ersip_uri:user(ersip_uri:make(<<"sips:bob@biloxi.com">>)).
%%    undefined = ersip_uri:user(ersip_uri:make(<<"sip:biloxi.com">>)).
%%    ersip_uri:user(ersip_uri:make(<<"tel:+16505550505">>)). % raises error
%% '''
-spec user(ersip_uri:uri()) -> binary() | undefined.
user(#uri{type = sip, data = D}) ->
    ersip_uri_sip:user(D);
user(#uri{} = URI) ->
    error({sip_uri_expected, URI}).

%% @doc Set user part of SIP URI.
%% Raises error if URI is not SIP(S) URI.
-spec set_user(binary(), ersip_uri:uri()) -> ersip_uri:uri().
set_user(NewUser, #uri{type = sip, data = D} = U) when is_binary(NewUser) ->
    U#uri{data = ersip_uri_sip:set_user(NewUser, D)};
set_user(NewUser, #uri{} = URI) when is_binary(NewUser)  ->
    error({sip_uri_expected, URI}).

%% @doc Get host part of SIP URI.
%% Raises error if URI is not SIP(S) URI.
-spec host(ersip_uri:uri()) -> ersip_host:host().
host(#uri{type = sip, data = D}) ->
    ersip_uri_sip:host(D);
host(#uri{} = URI) ->
    error({sip_uri_expected, URI}).

%% @doc Get host part of SIP URI in binary representation.
%% Raises error if URI is not SIP(S) URI.
%% Example:
%% ```
%%    <<"atlanta.com">> = ersip_uri:host_bin(ersip_uri:make(<<"sip:alice@atlanta.com">>)).
%%    <<"biloxi.com">> = ersip_uri:host_bin(ersip_uri:make(<<"sips:bob@biloxi.com">>)).
%%    <<"127.0.0.1">> = ersip_uri:host_bin(ersip_uri:make(<<"sip:127.0.0.1">>)).
%%    ersip_uri:host_bin(ersip_uri:make(<<"tel:+16505550505">>)). % raises error
%% '''
-spec host_bin(ersip_uri:uri()) -> binary().
host_bin(#uri{data = D} = U) ->
    force_sip(U),
    ersip_uri_sip:host_bin(D).

-spec set_host(ersip_host:host(), ersip_uri:uri()) -> ersip_uri:uri().
set_host(H, #uri{data = D} = U) ->
    force_sip(U),
    U#uri{data = ersip_uri_sip:set_host(H, D)}.

%% @doc Get port number of 'undefined'.
%% Raises error if URI is not SIP(S) URI.
%% ```
%%    undefined = ersip_uri:port(ersip_uri:make(<<"sip:alice@atlanta.com">>)).
%%    5060 = ersip_uri:port(ersip_uri:make(<<"sip:alice@atlanta.com:5060">>)).
%%    ersip_uri:port(ersip_uri:make(<<"tel:+16505550505">>)). %% raises error
%% '''
-spec port(ersip_uri:uri()) -> undefined | inet:port_number().
port(#uri{data = D} = U) ->
    force_sip(U),
    ersip_uri_sip:port(D).

%% @doc Set port number.
%% Raises error if URI is not SIP(S) URI.
-spec set_port(undefined | inet:port_number(), ersip_uri:uri()) -> ersip_uri:uri().
set_port(P, #uri{data = D} = U) when ?IS_VALID_PORT(P) ->
    force_sip(U),
    U#uri{data = ersip_uri_sip:set_port(P, D)}.

%% @doc Get transport from URI.
%% Raises error if URI is not SIP(S) URI.
-spec transport(ersip_uri:uri()) -> undefined | ersip_transport:transport().
transport(#uri{data = D} = U) ->
    force_sip(U),
    ersip_uri_sip:transport(D).

%% @doc Set transport to URI.
%% Raises error if URI is not SIP(S) URI.
-spec set_transport(ersip_transport:transport(), uri()) -> uri().
set_transport(Transport, #uri{data = D} = U) ->
    force_sip(U),
    U#uri{data = ersip_uri_sip:set_transport(Transport, D)}.

%% @doc Remove transport parameter from URI.
%% Raises error if URI is not SIP(S) URI.
-spec clear_transport(uri()) -> uri().
clear_transport(#uri{data = D} = U) ->
    force_sip(U),
    U#uri{data = ersip_uri_sip:clear_transport(D)}.

%% @doc Checks if URI has loose router parameter (lr).
%% Raises error if URI is not SIP(S) URI.
%% Example:
%% ```
%%   true  = ersip_uri:loose_route(ersip_uri:make(<<"sip:host;lr">>)).
%%   false = ersip_uri:loose_route(ersip_uri:make(<<"sip:host">>)).
%% '''
-spec loose_router(uri()) -> boolean().
loose_router(#uri{data = D} = U) ->
    force_sip(U),
    ersip_uri_sip:loose_router(D).

-spec set_loose_router(boolean(), uri()) -> uri().
set_loose_router(LR, #uri{data = D} = U) ->
    force_sip(U),
    U#uri{data = ersip_uri_sip:set_loose_router(LR, D)}.

%% @doc Return maddr parameter value or undefined.
%% Raises error if URI is not SIP(S) URI.
-spec maddr(uri()) -> ersip_host:host() | undefined.
maddr(#uri{data = D} = U) ->
    force_sip(U),
    ersip_uri_sip:maddr(D).

%% @doc Set maddr parameter value.
%% Raises error if URI is not SIP(S) URI or if first parameter is not
%% host.
-spec set_maddr(ersip_host:host(), uri()) -> uri().
set_maddr(Host, #uri{data = D} = U) ->
    force_sip(U),
    U#uri{data = ersip_uri_sip:set_maddr(Host, D)}.

%% @doc Remove maddr parameter from URI.
%% Raises error if URI is not SIP(S) URI.
-spec clear_maddr(uri()) -> uri().
clear_maddr(#uri{data = D} = U) ->
    force_sip(U),
    U#uri{data = ersip_uri_sip:clear_maddr(D)}.

%% @doc Get user parameter of URI (Ex: ;user=ip or ;user=phone).
%% Raises error if URI is not SIP(S) URI.
-spec user_param(uri()) -> ersip_uri_sip:user_param() | undefined.
user_param(#uri{data = D} = U) ->
    force_sip(U),
    ersip_uri_sip:user_param(D).

%% @doc Set user parameter of URI.
%% Raises error if URI is not SIP(S) URI.
-spec set_user_param(ersip_uri_sip:user_param(), uri()) -> uri().
set_user_param(UserParam, #uri{data = D} = U)
  when UserParam == ip; UserParam == phone; is_binary(UserParam) ->
    force_sip(U),
    U#uri{data = ersip_uri_sip:set_user_param(UserParam, D)};
set_user_param(UserParam, _) ->
    error({user_param_expected, UserParam}).

%% @doc Clear user parameter from URI.
%% Raises error if URI is not SIP(S) URI.
-spec clear_user_param(uri()) -> uri().
clear_user_param(#uri{data = D} = U) ->
    force_sip(U),
    U#uri{data = ersip_uri_sip:clear_user_param(D)}.

%% @doc Get ttl parameter of URI.
%% Raises error if URI is not SIP(S) URI.
-spec ttl(uri()) -> ersip_uri_sip:ttl_param() | undefined.
ttl(#uri{data = D} = U) ->
    force_sip(U),
    ersip_uri_sip:ttl(D).

%% @doc Set ttl parameter of URI.
%% Raises error if URI is not SIP(S) URI.
-spec set_ttl(ersip_uri_sip:ttl_param(), uri()) -> uri().
set_ttl(TTL, #uri{data = D} = U) ->
    force_sip(U),
    U#uri{data = ersip_uri_sip:set_ttl(TTL, D)}.

%% @doc Clear TTL parameter from URI.
%% Raises error if URI is not SIP(S) URI.
-spec clear_ttl(uri()) -> uri().
clear_ttl(#uri{data = D} = U) ->
    force_sip(U),
    U#uri{data = ersip_uri_sip:clear_ttl(D)}.

%% @doc Get generic parameter of the URI.
%% Raises error if URI is not SIP(S) URI.
%% This function also can be used to get known parameters in generic form
%% Example:
%% ```
%%   <<"11">> = ersip_uri:gen_param(<<"ttl">>, ersip_uri:make(<<"sip:b;ttl=11">>)).
%%   true = ersip_uri:gen_param(<<"lr">>, ersip_uri:make(<<"sip:b;lr">>)).
%%   undefined = ersip_uri:gen_param(<<"lr">>, ersip_uri:make(<<"sip:b">>)).
%% '''
-spec gen_param(binary(), uri()) -> binary() | true | undefined.
gen_param(Name, #uri{data = D} = U) ->
    force_sip(U),
    ersip_uri_sip:gen_param(Name, D).

%% @doc Set generic parameter of URI.
%% Raises error if URI is not SIP(S) URI.
-spec set_gen_param(binary(), binary() | true, uri()) -> uri().
set_gen_param(Name, Value, #uri{data = D} = U) ->
    force_sip(U),
    NewD = ersip_uri_sip:set_gen_param(Name, Value, D),
    U#uri{data = NewD}.

%% @doc Clear generic parameter of URI.
%% Raises error if URI is not SIP(S) URI.
-spec clear_gen_param(binary(), uri()) -> uri().
clear_gen_param(Name, #uri{data =  D} = U) ->
    force_sip(U),
    NewD = ersip_uri_sip:clear_gen_param(Name, D),
    U#uri{data = NewD}.

%% @doc Set parameter of the URI
%% @deprecated
%% This function is deprecated. Please use set_gen_param for generic
%% form and set_transport, set_ttl, set_... for known params.
-spec set_param(ersip_uri_sip:uri_param_name(), term(), uri()) -> uri().
set_param(Name, Value, #uri{data = D} = U) ->
    force_sip(U),
    NewD = ersip_uri_sip:set_param(Name, Value, D),
    U#uri{data = NewD}.

%% @doc Get URI part by identify. This function is deprecated and will
%% be removed eventually.
%% @deprecated
-spec get(uri_part_name() | [uri_part_name()], uri()) -> uri_part() | [uri_part()].
get(Part, URI) when is_atom(Part) ->
    get_part(Part, URI);
get(Parts, URI) when is_list(Parts) ->
    lists:map(fun(Part) ->
                      get_part(Part, URI)
              end,
              Parts).

%% @doc Create URI from binary, raw representation and deprecated from URI parts.
%% Note that creation from URI parts are deprecated and will be
%% removed in future releases.
%% Raises error if URI cannot be constracted from this data (has invalid syntax).
%% Examples:
%% ```
%%   SIPURI = ersip_uri:make(<<"sip:a@b">>),
%%   SIPURI = ersip_uri:make(#{scheme => <<"sip">>, data => <<"a@b">>}),
%%   TelURI = ersip_uri:make(<<"tel:+16505550505">>),
%%   TelURI = ersip_uri:make(#{scheme => <<"tel">>, data => <<"+16505550505">>}).
%% '''
-spec make(make() | [uri_part()]) -> uri().
make(Bin) when is_binary(Bin) ->
    case parse(Bin) of
        {ok, URI}       -> URI;
        {error, Reason} -> error(Reason)
    end;
make(#{scheme := Scheme, data := Data}) ->
    make(iolist_to_binary([Scheme, $:, Data]));
make(Parts) when is_list(Parts) ->
    Init = #uri{data = ersip_uri_sip:new()},
    lists:foldl(fun(Option, URI) ->
                        set_part(Option, URI)
                end,
                Init,
                Parts).

%% @doc Make URI comparable with =:= erlang operator.  This means that
%% if make_key(UriA) =:= make_key(UriB) then they equal by RFC3261 19.1.4 URI Comparison.
-spec make_key(uri()) -> uri().
make_key(#uri{} = URI) ->
    #uri{type = URI#uri.type,
         data = make_data_key(URI#uri.type, URI#uri.data)
        }.

%% @doc Parse URI from the binary
%% ```
%% SIP-URI          =  "sip:" [userinfo] hostport
%%                     uri-parameters [headers]
%% SIPS-URI         =  "sips:" [userinfo] hostport
%%                     uri-parameters [headers]
%% '''
-spec parse(binary()) -> parse_result().
parse(Binary) ->
     {Scheme, _} = ersip_uri_parser_aux:split_scheme(Binary),
    parse_uri(Scheme, Binary).

%% @doc Assemble URI to iolist.
-spec assemble(uri()) -> iolist().
assemble(#uri{type = T, data = Data}) ->
    assemble(T, Data).

%% @doc Assemble URI to binary.
-spec assemble_bin(uri()) -> binary().
assemble_bin(#uri{} = U) ->
    iolist_to_binary(assemble(U)).

%% @doc Get URI params.
%% @deprecated
-spec params(uri()) -> ersip_uri_sip:uri_params().
params(#uri{type = sip, data = D}) ->
    ersip_uri_sip:params(D);
params(#uri{}) ->
    #{}.

%% @doc Get raw URI params as list.
-spec raw_params(uri()) -> [{binary(), binary()} | binary()].
raw_params(#uri{data = D} = U) ->
    force_sip(U),
    ersip_uri_sip:raw_params(D).

%% @doc Get raw URI headers as list.
-spec raw_headers(uri()) -> [{binary(), binary()}].
raw_headers(#uri{data = D} = U) ->
    force_sip(U),
    ersip_uri_sip:raw_headers(D).

%% @doc Set raw URI headers from list.
-spec set_raw_headers([{binary(), binary()}], uri()) -> uri().
set_raw_headers(Headers, #uri{data = D} = U) ->
    force_sip(U),
    NewD = ersip_uri_sip:set_raw_headers(Headers, D),
    U#uri{data = NewD}.

%% @doc Clear all URI parameters.
-spec clear_params(uri()) -> uri().
clear_params(#uri{type = sip, data = D} = U) ->
    NewD = ersip_uri_sip:clear_all_params(D),
    U#uri{data = NewD};
clear_params(#uri{} = URI) ->
    URI.

%% @doc Set part of the URI
%% @deprecated
-spec set_part(uri_part(), uri()) -> uri().
set_part({user, U}, #uri{} = URI) when is_binary(U) ->
    set_user(U, URI);
set_part({port, P}, #uri{} = URI) when is_integer(P) ->
    set_port(P, URI);
set_part({host, H}, #uri{} = URI) ->
    case ersip_host:is_host(H) of
        true ->
            set_host(H, URI);
        false ->
            error({invalid_host, H})
    end;
set_part(Part, _) ->
    error({invalid_part, Part}).

%% @doc Get part of the URI
%% @deprecated
-spec get_part(uri_part_name(), uri()) -> uri_part().
get_part(scheme, #uri{} = URI) ->
    scheme(URI);
get_part(user, #uri{} = URI) ->
    {user, user(URI)};
get_part(port, #uri{} = URI) ->
    {port, port(URI)};
get_part(host, #uri{} = URI) ->
    {host, host(URI)}.

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
-spec clear_not_allowed_parts(Type, uri()) -> uri() when
      Type :: ruri
            | record_route.
clear_not_allowed_parts(T, #uri{type = sip, data = D} = URI) ->
    NewD = ersip_uri_sip:clear_not_allowed_parts(T, D),
    URI#uri{data = NewD};
clear_not_allowed_parts(_, URI) ->
    %% For schemes other than sip/sips we do not clear anything
    URI.

%% @doc Unquote and quote again headers.
-spec rebuild_header_values(uri()) -> uri().
rebuild_header_values(#uri{type = sip, data = D} = U) ->
    NewD = ersip_uri_sip:rebuild_header_values(D),
    U#uri{data = NewD};
rebuild_header_values(#uri{} = U) ->
    U.

%% @doc Get raw value (in plain erlang types) of the uri.
-spec raw(uri()) -> raw().
raw(#uri{type = T, data = D} = URI) ->
    Base = #{scheme => scheme_bin(URI),
             data => data(URI)},
    enrich_raw(T, D, Base).

%% @doc Returns true if URI is SIP or SIPS URI.
-spec is_sip(uri()) -> boolean().
is_sip(#uri{type = sip}) -> true;
is_sip(#uri{}) -> false.

-spec as_sip(uri()) -> ersip_uri_sip:sip_uri().
as_sip(#uri{type = sip, data = D}) -> D.

-spec from_sip(ersip_uri_sip:sip_uri()) -> uri().
from_sip(SIPData) ->
    #uri{type = sip, data = SIPData}.

-spec is_tel(uri()) -> boolean().
is_tel(#uri{type = tel}) -> true;
is_tel(#uri{}) -> false.

-spec as_tel(uri()) -> ersip_uri_tel:tel_uri().
as_tel(#uri{type = tel, data = D}) -> D.

-spec from_tel(ersip_uri_tel:tel_uri()) -> uri().
from_tel(TelData) ->
    #uri{type = tel, data = TelData}.

%%===================================================================
%% Internal implementation
%%===================================================================

-spec parse_uri(Scheme :: binary(), Bin :: binary()) -> parse_result().
parse_uri(<<"sip", _/binary>>, Bin) ->
    case ersip_uri_sip_parser:parse(Bin) of
        {ok, SipData} ->
            {ok, #uri{type = sip, data = SipData}};
        {error, Reason} ->
            {error, {invalid_sip_uri, Reason}}
    end;
parse_uri(<<"tel">>, Bin) ->
    case ersip_uri_tel:parse(Bin) of
        {ok, TelData} ->
            {ok, #uri{type = tel, data = TelData}};
        {error, Reason} ->
            {error, {invalid_tel_uri, Reason}}
    end;
parse_uri(_, Bin) ->
    case ersip_uri_absolute:parse(Bin) of
        {ok, AbsURI} ->
            {ok, #uri{type = absolute, data = AbsURI}};
        {error, Reason} ->
            {error, {invalid_abs_uri, Reason}}
    end.

-spec force_sip(uri()) -> ok.
force_sip(#uri{type = sip}) ->
    ok;
force_sip(#uri{} = URI) ->
    error({sip_uri_expected, URI}).


-spec make_data_key(type(), uri_data()) -> uri_data().
make_data_key(sip, SIPData) ->
    ersip_uri_sip:make_key(SIPData);
make_data_key(tel, TELData) ->
    ersip_uri_tel:make_key(TELData);
make_data_key(T, _) ->
    error({cannot_make_key, {unsupported_type, T}}).


assemble_scheme({scheme, sip}) ->
    <<"sip">>;
assemble_scheme({scheme, sips}) ->
    <<"sips">>;
assemble_scheme({scheme, tel}) ->
    <<"tel">>;
assemble_scheme({scheme, Scheme}) ->
    Scheme.

-spec assemble(type(), uri_data()) -> iolist().
assemble(sip, SIPData) ->
    ersip_uri_sip:assemble(SIPData);
assemble(tel, TELData) ->
    ersip_uri_tel:assemble(TELData);
assemble(absolute, AbsData) ->
    ersip_uri_absolute:assemble(AbsData).

-spec enrich_raw(type(), uri_data(), raw()) -> raw().
enrich_raw(sip, SIPData, Base) ->
    ersip_uri_sip:enrich_raw(SIPData, Base);
enrich_raw(_, _, Base) ->
    Base.
