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
-spec parse(binary()) -> {ok, host()} | {error, einval}.
parse(<<$[, _/binary>> = R) ->
    parse_ipv6_reference(R);
parse(<<Char/utf8, _/binary>> = R) when ?is_DIGIT(Char) ->
    parse_ipv4_address(R);
parse(Bin) when is_binary(Bin) ->
    case hostname_valid(Bin) of
        true ->
            {ok, {hostname, Bin}};
        false ->
            {error, einval}
    end.

%% @doc make comparable hostname (from rfc3261 comparision rules).
-spec make_key(host()) -> host().
make_key({hostname, Bin}) ->
    {hostname, ersip_bin:to_lower(ersip_bin:unquote_rfc_2396(Bin))};
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
              | {error, einval}.
parse_ipv6_reference(<<$[, R/binary>>) when R =/= <<>> ->
    case binary:at(R, byte_size(R)-1) of
        $] ->
            parse_ipv6_address(binary:part(R, 0, byte_size(R)-1));
        _ ->
            {error, einval}
    end.

%% @private
%% @doc Parse IPv6 address
%%
%% IPv6reference  =  "[" IPv6address "]"
%% IPv6address    =  hexpart [":" IPv4address]
%% hexpart        =  hexseq / hexseq "::" [hexseq] / "::" [hexseq]
%% hexseq         =  hex4 *( ":" hex4)
%% hex4           =  1*4HEXDIG
-spec parse_ipv6_address(binary()) -> {ok, {ipv6, inet:ip6_address()}} | {error, einval}.
parse_ipv6_address(Bin) when is_binary(Bin) ->
    L = binary_to_list(Bin),
    case inet:parse_address(L) of
        {ok, {_, _, _, _,   _, _, _, _} = Addr} ->
            {ok, {ipv6, Addr}};
        _ ->
            {error, einval}
    end.

%% @private
%% @doc Parse IPv4 address
%%
%% IPv4address    =  1*3DIGIT "." 1*3DIGIT "." 1*3DIGIT "." 1*3DIGIT
-spec parse_ipv4_address(binary()) -> {ok, host()} | {error, einval}.
parse_ipv4_address(Bin) when is_binary(Bin) ->
    L = binary_to_list(Bin),
    case inet:parse_address(L) of
        {ok, {_, _, _, _} = Addr} ->
            {ok, {ipv4, Addr}};
        _ ->
            {error, einval}
    end.

%% @private
%% @doc Check that hostname agree specicification
%%
%% hostname         =  *( domainlabel "." ) toplabel ["."]
%%
-spec hostname_valid( binary() | [binary()] ) -> boolean().
hostname_valid(Bin) when is_binary(Bin) ->
    hostname_valid(binary:split(Bin, <<".">>, [global]));
hostname_valid([<<>>]) ->
    false;
hostname_valid([TopLabel, <<>>]) ->
    toplabel_valid(TopLabel, start);
hostname_valid([TopLabel]) ->
    toplabel_valid(TopLabel, start);
hostname_valid([DomainLabel | Rest]) ->
    case domainlabel_valid(DomainLabel, start) of
        false ->
            false;
        true ->
            hostname_valid(Rest)
    end.

%% @doc Check that toplabel agree specicification
%%
%% toplabel         =  ALPHA / ALPHA *( alphanum / "-" )
%% @private
-spec toplabel_valid( binary(), start | rest ) -> boolean().
toplabel_valid(<<>>, start) ->
    false;
toplabel_valid(<<>>, rest) ->
    true;
toplabel_valid(<<Char/utf8, R/binary>>, start) when ?is_ALPHA(Char) ->
    toplabel_valid(R, rest);
toplabel_valid(_, start) ->
    false;
toplabel_valid(<<Char/utf8, R/binary>>, rest) when ?is_alphanum(Char) ->
    toplabel_valid(R, rest);
toplabel_valid(_, rest) ->
    false.

%% @private
%% @doc Check that domainlabel agree specicification
%% domainlabel      =  alphanum
%%                     / alphanum *( alphanum / "-" ) alphanum
-spec domainlabel_valid( binary(), start | rest ) -> boolean().
domainlabel_valid(<<>>, start) ->
    false;
domainlabel_valid(<<Char/utf8>>, start) when ?is_alphanum(Char) ->
    true;
domainlabel_valid(<<Char/utf8, R/binary>>, start) when ?is_alphanum(Char) ->
    domainlabel_valid(R, rest);
domainlabel_valid(<<Char/utf8>>, rest) when ?is_alphanum(Char) ->
    true;
domainlabel_valid(<<_/utf8>>, rest)  ->
    false;
domainlabel_valid(<<Char/utf8, R/binary>>, rest) when ?is_alphanum(Char) orelse Char =:= $- ->
    domainlabel_valid(R, rest);
domainlabel_valid(_, _) ->
    false.

-spec assure_host(MaybeHost) -> host() when
      MaybeHost :: term().
assure_host(Host) ->
    case is_host(Host) of
        true ->
            Host;
        false ->
            error({error, {not_valid_host, Host}})
    end.
