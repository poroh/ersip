%%
%% Copyright (c) 2017 Dmitry Poroh
%% All rights reserved.
%% Distributed under the terms of the MIT License. See the LICENSE file.
%%
%% Host-related routines
%%

-module(ersip_host).
-export([is_host/1,
         parse/1,
         make/1,
         make_key/1,
         assemble/1
        ]).
-export_type([host/0]).

%%%===================================================================
%%% Types
%%%===================================================================

-type host() :: hostname()
              | address().
-type address()  :: {ipv4, inet:ip4_address()}
                  | {ipv6, inet:ip6_address()}.
-type hostname() :: {hostname, binary()}.

-type parse_result() :: {ok, host()}
                      | {error, {invalid_host, binary()}}
                      | {error, {invalid_ipv6, binary()}}.

%%%===================================================================
%%% API
%%%===================================================================

-include("ersip_sip_abnf.hrl").

%% @doc check term is valid host.
-spec is_host(MaybeHost) -> boolean() when
      MaybeHost :: host()
                 | term().
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

%% @doc Generate host specification from binary.
-spec parse(binary()) -> parse_result().
parse(<<$[, _/binary>> = R) ->
    parse_ipv6_reference(R);
parse(<<Char/utf8, _/binary>> = R) when ?is_HEXDIG(Char) ->
    case parse_address(R) of
        {ok, _} = Result -> Result;
        {error, {invalid_address, _}}  -> %% second try
            assume_its_hostname(R)
    end;
parse(Bin) when is_binary(Bin) ->
    assume_its_hostname(Bin).

%% @doc make comparable hostname (from rfc3261 comparision rules).
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

-spec assemble(host()) -> iolist().
assemble({hostname, Bin}) ->
    Bin;
assemble({ipv4, IpAddr}) ->
    inet:ntoa(IpAddr);
assemble({ipv6, IpAddr}) ->
    [$[, inet:ntoa(IpAddr), $]].

-spec make(Addr) -> host() when
      Addr :: inet:ip_address()
            | binary().
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
        {ok, Host} ->
            Host;
        {error, _} ->
            error({error, {invalid_host, Addr}})
    end.

%%%===================================================================
%%% Internal implementation
%%%===================================================================

-type nonempty_binary() :: <<_:8,_:_*8>>.

%% @private
%% @doc Parse IPv6 reference
%%
%% IPv6reference  =  "[" IPv6address "]"
-spec parse_ipv6_reference(nonempty_binary()) -> Result when
      Result :: {ok, {ipv6, inet:ip6_address()}}
              | {error, {invalid_host, binary()}}.
parse_ipv6_reference(R) when byte_size(R) < 2 ->
    {error, {invalid_host, R}};
parse_ipv6_reference(R) ->
    IP6size = byte_size(R) - 2,
    case R of
        <<"[", IP6:IP6size/binary, "]">> ->  parse_ipv6_address(IP6);
        _ ->  {error, {invalid_host, R}}
    end.

%% @private
%% @doc Parse IPv6 address
%%
%% IPv6reference  =  "[" IPv6address "]"
%% IPv6address    =  hexpart [":" IPv4address]
%% hexpart        =  hexseq / hexseq "::" [hexseq] / "::" [hexseq]
%% hexseq         =  hex4 *( ":" hex4)
%% hex4           =  1*4HEXDIG
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
%% IPv4address    =  1*3DIGIT "." 1*3DIGIT "." 1*3DIGIT "." 1*3DIGIT
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
domainlabel_valid([$., A])         when ?is_alphanum(A) -> true;
domainlabel_valid([A])             when ?is_alphanum(A) -> true;
domainlabel_valid([$., A | R])     when ?is_alphanum(A) -> domainlabel_valid(R);
domainlabel_valid([A, $. | _] = R) when ?is_alphanum(A) -> domainlabel_valid(tl(R));
%% domainlabel body
domainlabel_valid([$- | R])                             -> domainlabel_valid(R);
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

-spec assume_its_hostname(binary()) -> {ok, host()} | {error, {invalid_host, binary()}}.
assume_its_hostname(Bin) ->
    case hostname_valid(Bin) of
        true ->
            {ok, {hostname, Bin}};
        false ->
            {error, {invalid_host, Bin}}
    end.
