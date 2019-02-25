-module(prop_basic).
-include_lib("proper/include/proper.hrl").
-include("ersip_sip_abnf.hrl").
-include("ersip_uri.hrl").
-include("ersip_headers.hrl").
-compile([export_all,
          {no_auto_import,[date/0, time/0]}]).


%%%%%%%%%%%%%%%%%%
%%% Properties %%%
%%%%%%%%%%%%%%%%%%

prop_is_assemble_parse_equal() ->
    ?FORALL(Msg, message(),
        begin
            Bin1 = ersip_sipmsg:assemble_bin(Msg),
            {ok, Msg1} = ersip_sipmsg:parse(Bin1, all),
            Bin2 = ersip_sipmsg:assemble_bin(Msg1),
            Bin1 == Bin2
        end).

%%%%%%%%%%%%%%%
%%% Helpers %%%
%%%%%%%%%%%%%%%
with_headers(Msg) ->
    ?LET({Headers1, Headers2, Via},
         {required_headers(ersip_sipmsg:method(Msg)), optional_headers(ersip_sipmsg:method(Msg)), hdr_via()},
         begin
             SipMsg0 = lists:foldl(fun({Key, Val}, Acc) ->
                                           ersip_sipmsg:set(Key, Val, Acc)
                                   end, Msg, Headers1 ++ Headers2),

             NewRawMsg = ersip_msg:set_header(Via, ersip_sipmsg:raw_message(SipMsg0)),
             ersip_sipmsg:set_raw_message(NewRawMsg, SipMsg0)
         end).

%%%%%%%%%%%%%%%%%%
%%% Generators %%%
%%%%%%%%%%%%%%%%%%
message() ->
    oneof([message_req(), message_resp()]).

message_req() ->
    ?LET({Method, RURI},
         {method(), uri()},
         with_headers(ersip_sipmsg:new_request(Method, RURI))).

message_resp() ->
    ?LET({Status, Reason, Method},
         {range(100, 699), utf8(), method()},
         with_headers(ersip_sipmsg:new_reply(Status, Reason, Method))).

method() ->
    oneof([ersip_method:options(),
           ersip_method:invite(),
           ersip_method:ack(),
           ersip_method:bye(),
           ersip_method:cancel(),
           ersip_method:subscribe(),
           ersip_method:notify(),
           ersip_method:refer(),
           ersip_method:register()]).

%% ---- HEADERS Gen ----
required_headers(Method) ->
    ?LET({From, To, CallId, CSeq, Event, ReferTo},
         {hdr_from(), hdr_to(), hdr_callid(), hdr_cseq(Method), hdr_event(), hdr_refer_to()},
         begin
             Common = [From, To, CallId, CSeq],
             Subscribe = ersip_method:subscribe(),
             Notify = ersip_method:notify(),
             Refer = ersip_method:refer(),
             Specific = case Method of
                            Subscribe -> [Event];
                            Notify -> [Event];
                            Refer  -> [ReferTo];
                            _    -> []
                        end,
             Common ++ Specific
         end).

optional_headers(Method) ->
    proper_types:resize(10, list(optional_header(Method))).

optional_header(Method) ->
    oneof([hdr_maxforwards(),
           hdr_date(),
           hdr_allow(),
           hdr_expires(),
           hdr_contact_list(Method),
           hdr_content_type()]).

hdr_refer_to() ->
    ?LET({URI, DN, Params},
         {uri(), display_name(), hparams()},
         {refer_to, ersip_hdr_refer_to:new(URI, DN, Params)}).
hdr_from() ->
    ?LET(From, fromto_hd(), {from, From}).
hdr_to() ->
    ?LET(To, fromto_hd(), {to, To}).
hdr_callid() ->
    ?LET(ID, word(), {callid, ersip_hdr_callid:make(ID)}).
hdr_via() ->
    ?LET({Host,   Port,   Transp},
         {host(), port(), transport()},
         begin
             Via = ersip_hdr_via:new(Host, Port, Transp),
             ViaH = ersip_hdr:new(<<"Via">>),
             ersip_hdr:add_topmost(ersip_hdr_via:assemble(Via), ViaH)
         end) .
hdr_cseq(Method) ->
    ?LET(N, non_neg_integer(),
         {cseq, ersip_hdr_cseq:make(Method, N)}).
hdr_maxforwards() ->
    ?LET(N, non_neg_integer(),
         {maxforwards, ersip_hdr_maxforwards:make(N)}).
hdr_event() ->
    ?LET({Event, Id}, {alphanum(), alphanum()},
         {event, ersip_hdr_event:new(Event, Id)}).
hdr_date() ->
    ?LET({Date, Time}, {date(), time()},
         {date, ersip_hdr_date:make(Date, Time)}).
hdr_allow() ->
    ?LET(Methods, non_empty(list(method())),
         {allow, ersip_hdr_allow:from_list(Methods)}).
hdr_expires() ->
    ?LET(N, range(0, 4294967295),
         {expires, ersip_hdr_expires:make(N)}).
hdr_contact_list(Method) ->
    Invite = ersip_method:invite(),
    Notify = ersip_method:notify(),
    case Method of
        Invite -> {contact, [hdr_contact()]};
        Notify -> {contact, [hdr_contact()]};
        _      -> {contact, proper_types:resize(5, list(hdr_contact()))} %up to 5 Contact's
    end.
hdr_content_type() ->
    ?LET({Type, SubType, Params},
         {token(), token(), list(gen_param())},
         {content_type, ersip_hdr_content_type:new(ersip_hdr_content_type:make_mime_type(Type, SubType), Params)}).


%% it's dummy headers
hdr_contact() ->
    ?LET({URI, Exp, QVal, Params},
         {uri(), non_neg_integer(), qval(), list(?SUCHTHAT({Key, _}, gen_param(), case Key of
                                                                                      <<"q">> -> false;
                                                                                      <<"Q">> -> false;
                                                                                      <<"Expires">> -> false;
                                                                                      <<"expires">> -> false;
                                                                                      <<"EXPIRES">> -> false;
                                                                                      _ -> true
                                                                                  end))},
         begin
             C1 = ersip_hdr_contact:new(URI),
             C2 = ersip_hdr_contact:set_expires(Exp, C1),
             C3 = ersip_hdr_contact:set_qvalue(QVal, C2),
             lists:foldl(fun({PKey, PVal}, Acc) ->
                                 ersip_hdr_contact:set_param(PKey, PVal, Acc)
                         end, C3, Params)
         end).
fromto_hd() ->
    ?LET({DN, URI, Tag},
         {display_name(), uri(), token()},
         ersip_hdr_fromto:set_tag({tag, Tag}, ersip_hdr_fromto:new(URI, DN))).

%% ------- Complex data Gen -----------------
gen_param() ->
    {token(), token()}.

hparams() ->
    ?LET(Params,
         list(gen_param()),
         lists:foldl(fun({PKey, PVal}, Acc) ->
                             ersip_hparams:set_raw(PKey, PVal, Acc)
                     end, ersip_hparams:new(), Params)).


qval() ->
    %% ?LET(QVal, range(0, 1000), {qvalue, integer_to_binary(QVal)}).
    {qvalue, range(0, 1000)}.

uri() ->
    ?LET({Scheme,       Data      },
         {uri_scheme(), uri_data()},
         #uri{scheme = Scheme,
              data = Data}).

uri_scheme() ->
    {scheme, oneof([sip, sips])}.

uri_data() ->
    oneof([sip_uri_data(), absolute_uri_data()]).

sip_uri_data() ->
    #sip_uri_data{user = oneof([undefined, {user, alphanum()}]),
                  host = host(),
                  port = maybe(port()),
                  params = uri_params(),
                  headers = uri_headers()}.

absolute_uri_data() ->
    ?LET({Host, Domain}, {alpha(), alpha()}, #absolute_uri_data{opaque = <<Host/binary, $., Domain/binary>>}).

host() ->
    oneof([hostname(), address()]).

address() ->
    oneof([ip4_address(), ip6_address()]).

hostname() ->
    ?LET({Host, Domain}, {alpha(), alpha()}, {hostname, <<Host/binary, $., Domain/binary>>}).

ip4_address() ->
    {ipv4, {range(0,255), range(0,255), range(0,255), range(0,255)}}.

ip6_address() ->
    {ipv6, {range(0,65535), range(0,65535),
            range(0,65535), range(0,65535),
            range(0,65535), range(0,65535),
            range(0,65535), range(0,65535)}}.

uri_params() ->
    ?LET({Transport,   Maddr,  Ttl,          User,                           Method,     Lr       },
         {transport(), host(), range(0,255), oneof([phone, ip, alphanum()]), alphanum(), boolean()},
         #{transport => Transport,
           maddr     => Maddr,
           ttl       => Ttl,
           user      => User,
           method    => Method,
           lr        => Lr}).

uri_headers() ->
    ?LET(List, list({alphanum(), alphanum()}), maps:from_list(List)).

transport() ->
    oneof([known_transport()%% , other_transport()
          ]).

known_transport() ->
    {transport, oneof([udp,tcp,tls,ws,wss])}.

other_transport() ->
    {other_transport, alphanum()}.

display_name() ->
    {display_name, oneof([alphanum(), list(alphanum())])}.


%% ------- Primitive data Gen -----------------
token() ->
    ?LET(T, non_empty(list(token_char())), iolist_to_binary(T)).
token_char() ->
    ?SUCHTHAT(C, byte(), ?is_token_char(C)).

alphanum() ->
    ?LET(A, non_empty(list(alphanum_char())), iolist_to_binary(A)).
alphanum_char() ->
    ?SUCHTHAT(C, byte(), ?is_alphanum(C)).

alpha() ->
    ?LET(A, non_empty(list(alpha_char())), iolist_to_binary(A)).
alpha_char() ->
    ?SUCHTHAT(C, byte(), ?is_ALPHA(C)).

word() ->
    ?LET(W, non_empty(list(word_char())), iolist_to_binary(W)).

word_char() ->
    oneof([alphanum_char(),<<$->>,<<$.>>,<<$!>>,<<$%>>,<<$*>>,<<$_>>,<<$+>>,<<$`>>,<<$'>>,<<$~>>,<<$(>>,<<$)>>,<<$<>>,<<$>>>,<<$:>>,<<$\\>>,<<$">>,<<$/>>,<<$[>>,<<$]>>,<<$?>>,<<${>>,<<$}>>]).

maybe(Gen) ->
    oneof([undefined, Gen]).

port() ->
    range(1, 65535).

%% http://erlang.org/doc/man/calendar.html#type-date
date() ->
    ?SUCHTHAT(D, {non_neg_integer(), range(1,12), range(1,31)}, calendar:valid_date(D)).

%% http://erlang.org/doc/man/calendar.html#type-time
time() ->
    {range(0,23), range(0,59), range(0,59)}.
