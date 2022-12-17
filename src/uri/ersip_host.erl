%%%
%%% Copyright (c) 2017, 2020 Dmitry Poroh
%%% All rights reserved.
%%% Distributed under the terms of the MIT License. See the LICENSE file.
%%%
%%% Host-related routines
%%%

-module(ersip_host).
-export([is_host/1,
         is_ip_address/1,
         ip_address/1,
         check_hostname/1,
         parse/1,
         make/1,
         make_key/1,
         assemble/1,
         assemble_bin/1,
         assemble_received/1,
         raw/1
        ]).
-export_type([host/0, raw/0, parse_error/0]).

%%===================================================================
%% Types
%%===================================================================

-type host() :: hostname()
              | address().
-type address()  :: {ipv4, inet:ip4_address()}
                  | {ipv6, inet:ip6_address()}.
-type hostname() :: {hostname, binary()}.

-type parse_result() :: {ok, host()} | {error, parse_error()}.
-type parse_error() :: {invalid_name, binary()}
                     | {invalid_ipv6, binary()}.
-type raw() :: binary().

%%===================================================================
%% API
%%===================================================================

-include("ersip_sip_abnf.hrl").

%% @doc Check term is valid host.
-spec is_host(host() | term()) -> boolean().
is_host({hostname, Bin}) ->
    hostname_valid(Bin);
is_host({ipv4, {A0,A1,A2,A3}}) ->
    lists:all(fun(X) ->
                      X >= 0 andalso X =< 255
              end,
              [A0,A1,A2,A3]);
is_host({ipv6, {A0,A1,A2,A3,A4,A5,A6,A7}}) ->
    lists:all(fun(X) ->
                      X >= 0 andalso X =< 65535
              end,
              [A0,A1,A2,A3,A4,A5,A6,A7]);
is_host(_) ->
    false.

%% @doc Check if host is defined as IP address.
-spec is_ip_address(host()) -> boolean().
is_ip_address({ipv4, _}) -> true;
is_ip_address({ipv6, _}) -> true;
is_ip_address({hostname, _}) -> false.

%% @doc Transform host to inet:ip_address().
%% Raises error if not ersip_host:is_ip_address().
-spec ip_address(host()) -> inet:ip_address().
ip_address({ipv4, A}) -> A;
ip_address({ipv6, A}) -> A;
ip_address({hostname, _}) -> error({api_error, <<"cannot get IP from hostname host. Use resolve.">>}).

%% @doc Check that hostname is valid domain name.
-spec check_hostname(binary()) -> boolean().
check_hostname(Bin) when is_binary(Bin) ->
    hostname_valid(Bin).

%% @doc Generate host specification from binary.
-spec parse(binary()) -> ersip_parser_aux:parse_result(host()).
parse(Binary) ->
    Pos = find_host_end(Binary, 0),
    <<MaybeHost:Pos/binary, Rest/binary>> = Binary,
    case do_parse(MaybeHost) of
        {ok, Host} ->
            {ok, Host, Rest};
        {error, _} = Error ->
            Error
    end.

%% @doc Make comparable hostname (from rfc3261 comparision rules).
-spec make_key(host()) -> host().
make_key({hostname, Bin}) ->
    UnquotedLowerHostName = ersip_bin:to_lower(ersip_bin:unquote_rfc_2396(Bin)),
    Len = byte_size(UnquotedLowerHostName),
    LastChar = binary:at(UnquotedLowerHostName, Len-1),
    NoTopDomain =
        case LastChar of
            $. ->
                binary:part(Bin, {0, Len-1});
            _ ->
                UnquotedLowerHostName
        end,
    {hostname, NoTopDomain};
make_key({ipv4, _} = H) ->
    H;
make_key({ipv6, _} = H) ->
    H.

%% @doc Assemble hostname to iolist().
-spec assemble(host()) -> iolist().
assemble({hostname, Bin}) ->
    Bin;
assemble({ipv4, IpAddr}) ->
    inet:ntoa(IpAddr);
assemble({ipv6, IpAddr}) ->
    [$[, assemble_ip6(IpAddr), $]].

%% @doc Assemble hostname to binary().
-spec assemble_bin(host()) -> binary().
assemble_bin(Host) ->
    iolist_to_binary(assemble(Host)).

%% @doc Assemble host as received parameter of via.
%% ```
%% via-received      =  "received" EQUAL (IPv4address / IPv6address)
%% '''
-spec assemble_received(host()) -> iolist().
assemble_received({hostname, Bin}) ->
    Bin;
assemble_received({ipv4, IpAddr}) ->
    inet:ntoa(IpAddr);
assemble_received({ipv6, IpAddr}) ->
    assemble_ip6(IpAddr).

%% @doc Create hostname from inet:ip_address() or from another host or
%% from raw().
-spec make(inet:ip_address() | host() | raw()) -> host().
make({_, _, _, _} = Addr) ->
    assure_host({ipv4, Addr});
make({ipv4, {_, _, _, _}} = Addr) ->
    assure_host(Addr);
make({_, _, _, _,  _, _, _, _} = Addr) ->
    assure_host({ipv6, Addr});
make({ipv6, {_, _, _, _,  _, _, _, _}} = Addr) ->
    assure_host(Addr);
make({hostname, _} = Host) ->
    assure_host(Host);
make(Addr) when is_binary(Addr)  ->
    case parse(Addr) of
        {ok, Host, <<>>} ->
            Host;
        {error, _} ->
            error({error, {invalid_name, Addr}})
    end.

%% @doc Get raw value (in plain erlang types) of the host.
-spec raw(host()) -> raw().
raw(Host) ->
    assemble_bin(Host).

%%===================================================================
%% Internal implementation
%%===================================================================

-if(?OTP_RELEASE < 25).
-type nonempty_binary() :: <<_:8,_:_*8>>.
-endif.

-define(VALID_HOST_CHAR(C), (?is_alphanum(C)
                             orelse C == $.
                             orelse C == $-
                             orelse C == $_
                             orelse C == $:
                             orelse C == $[
                             orelse C == $])).

-spec do_parse(binary()) -> parse_result().
do_parse(<<$[, _/binary>> = R) ->
    parse_ipv6_reference(R);
do_parse(<<Char/utf8, _/binary>> = R) when ?is_HEXDIG(Char) ->
    case parse_address(R) of
        {ok, _} = Result -> Result;
        {error, {invalid_address, _}}  -> %% second try
            assume_its_hostname(R)
    end;
do_parse(Bin) when is_binary(Bin) ->
    assume_its_hostname(Bin).

-spec find_host_end(binary(), non_neg_integer()) -> non_neg_integer().
find_host_end(<<Char, Rest/binary>>, Pos) when ?VALID_HOST_CHAR(Char) ->
    find_host_end(Rest, Pos+1);
find_host_end(_, Pos) ->
    Pos.

%% @private
%% @doc Parse IPv6 reference
%% ```
%% IPv6reference  =  "[" IPv6address "]"
%% '''
-spec parse_ipv6_reference(nonempty_binary()) -> Result when
      Result :: {ok, {ipv6, inet:ip6_address()}}
              | {error, parse_error()}.
parse_ipv6_reference(R) when byte_size(R) < 2 ->
    {error, {invalid_name, R}};
parse_ipv6_reference(R) ->
    IP6size = byte_size(R) - 2,
    case R of
        <<"[", IP6:IP6size/binary, "]">> ->  parse_ipv6_address(IP6);
        _ ->  {error, {invalid_name, R}}
    end.

%% @private
%% @doc Parse IPv6 address
%%
%% ```
%% IPv6reference  =  "[" IPv6address "]"
%% IPv6address    =  hexpart [":" IPv4address]
%% hexpart        =  hexseq / hexseq "::" [hexseq] / "::" [hexseq]
%% hexseq         =  hex4 *( ":" hex4)
%% hex4           =  1*4HEXDIG
%% '''
-spec parse_ipv6_address(binary()) -> {ok, {ipv6, inet:ip6_address()}} | {error, {invalid_ipv6, binary()}}.
parse_ipv6_address(Bin) when is_binary(Bin) ->
    L = binary_to_list(Bin),
    case inet:parse_ipv6strict_address(L) of
        {ok, {_, _, _, _,  _, _, _, _} = Addr} ->
            {ok, {ipv6, Addr}};
        _ ->
            {error, {invalid_ipv6, Bin}}
    end.

%% @private
%% @doc Parse IPv4 address
%%
%% ```
%% IPv4address    =  1*3DIGIT "." 1*3DIGIT "." 1*3DIGIT "." 1*3DIGIT
%% '''
-spec parse_address(binary()) -> {ok, host()} | {error, {invalid_address, binary()}}.
parse_address(Bin) when is_binary(Bin) ->
    L = binary_to_list(Bin),
    case inet:parse_address(L) of
        {ok, {_, _, _, _} = Addr} ->
            {ok, {ipv4, Addr}};
        {ok, {_, _, _, _,  _, _, _, _} = Addr} ->
            {ok, {ipv6, Addr}};
        _ ->
            {error, {invalid_address, Bin}}
    end.

%% @private
%% @doc Check that hostname agree specicification
%%
-spec hostname_valid( binary() ) -> boolean().

%% 'Jewish' trick. Some grammars much easier parse from right to left.
%% For instance in case of hostname parsing from left to right, we dont know if we parse domainlabel or toplabel.
%%  In case parsing right to left, toplabel will be parsed first, and domainlabels are all the rest.
%%
%%  So. Instead of
%%
%%  hostname         =  *( domainlabel "." ) toplabel ["."]
%%  domainlabel      =  alphanum  / alphanum *( alphanum / "-" ) alphanum
%%  toplabel         =  ALPHA / ALPHA *( alphanum / "-" )
%%
%%  I reverse chars in the string and parse
%%  [.] alphanum *("-"/alphanum) alpha *("." domainlabel)

-compile({inline, [toplabel_valid/1, domainlabel_valid/1]}).
hostname_valid(B) ->
    Reversed = lists:reverse(binary_to_list(B)),
    case Reversed  of
       [$., A | _ ] when ?is_alphanum(A) ->
            toplabel_valid(tl(Reversed));
       [A | _ ] when ?is_alphanum(A) ->
            toplabel_valid(Reversed);
        _ ->
            false
    end.

%% border conditions for toplabel
toplabel_valid([A]) when ?is_ALPHA(A)                -> true;
toplabel_valid([A, $. | _] = L) when ?is_ALPHA(A)    -> domainlabel_valid(tl(L));
%% toplabel body
toplabel_valid([$- | Rest])                          -> toplabel_valid(Rest);
toplabel_valid([A  | Rest])     when ?is_alphanum(A) -> toplabel_valid(Rest);
%% something wrong
toplabel_valid(_)                                    -> false.

%% border conditions for domainlabels
domainlabel_valid([$., $_])                             -> true;
domainlabel_valid([$., A])         when ?is_alphanum(A) -> true;
domainlabel_valid([$_])                                 -> true;
domainlabel_valid([A])             when ?is_alphanum(A) -> true;
domainlabel_valid([$., $_ | R])                         -> domainlabel_valid(R);
domainlabel_valid([$., A | R])     when ?is_alphanum(A) -> domainlabel_valid(R);
domainlabel_valid([$_, $. | _] = R)                     -> domainlabel_valid(tl(R));
domainlabel_valid([A, $. | _] = R) when ?is_alphanum(A) -> domainlabel_valid(tl(R));
%% domainlabel body
domainlabel_valid([$- | R])                             -> domainlabel_valid(R);
domainlabel_valid([$_ | R])                             -> domainlabel_valid(R);
domainlabel_valid([A  | R]) when ?is_alphanum(A)        -> domainlabel_valid(R);
%% something wrong
domainlabel_valid(_)                                    -> false.

-spec assure_host(MaybeHost) -> host() when
      MaybeHost :: term().
assure_host(Host) ->
    case is_host(Host) of
        true ->
            Host;
        false ->
            error({error, {not_valid_host, Host}})
    end.

-spec assume_its_hostname(binary()) -> {ok, host()} | {error, {invalid_name, binary()}}.
assume_its_hostname(Bin) ->
    case hostname_valid(Bin) of
        true ->
            {ok, {hostname, Bin}};
        false ->
            {error, {invalid_name, Bin}}
    end.

-spec assemble_ip6(inet:ip6_address()) -> iolist().
assemble_ip6({A0, A1, A2, A3, A4, A5, A6, A7}) ->
    {Head, Tail, State} = split_ip6([A0, A1, A2, A3, A4, A5, A6, A7], {[], [], head}),
    case State of
        head ->
            ersip_iolist:join($:, [assemble_ip6_hexpart(X) || X <- Head]);
        _ ->
            [ersip_iolist:join($:, [assemble_ip6_hexpart(X) || X <- Head]),
             "::", ersip_iolist:join($:, [assemble_ip6_hexpart(X) || X <- Tail])]
    end.

-spec split_ip6([0..65535], Acc) -> Acc when
      Acc :: {[0..65535],[0..65535], head | mid0 | tail}.
split_ip6([], {H, T, State}) ->
    {lists:reverse(H), lists:reverse(T), State};
split_ip6([0 | Rest], {H, [], head}) -> split_ip6(Rest, {H,       [], mid0});
split_ip6([X | Rest], {H, [], head}) -> split_ip6(Rest, {[X | H], [], head});
split_ip6([0 | Rest], {H, [], mid0}) -> split_ip6(Rest, {H,       [], mid0});
split_ip6([X | Rest], {H, [], mid0}) -> split_ip6(Rest, {H,      [X], tail});
split_ip6([X | Rest], {H,  T, tail}) -> split_ip6(Rest, {H,  [X | T], tail}).


-spec assemble_ip6_hexpart(0..65536) -> string().
assemble_ip6_hexpart(X) ->
    io_lib:format("~.16.0B", [X]).
