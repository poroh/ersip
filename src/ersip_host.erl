%%
%% Copyright (c) 2017 Dmitry Poroh
%% All rights reserved.
%% Distributed under the terms of the MIT License. See the LICENSE file.
%%
%% Host-related routines
%%

-module(ersip_host).
-export([ is_host/1, parse/1 ]).

-include("ersip_sip_abnf.hrl").


-type host() :: { hostname, binary() }
              | { ipv4,     inet:ip4_address() }
              | { ipv6,     inet:ip6_address() }.

%%%===================================================================
%%% API
%%%===================================================================


%% @doc check term is valid host.
-spec is_host(term()) -> boolean().
is_host({ hostname, Bin }) ->
    hostname_valid(Bin);
is_host({ ipv4, {A0,A1,A2,A3} }) ->
    lists:all(fun(X) ->
                      X >= 0 andalso X =< 255
              end,
              [ A0,A1,A2,A3 ]);
is_host({ ipv6, {A0,A1,A2,A3,A4,A5,A6,A7} }) ->
    lists:all(fun(X) ->
                      X >= 0 andalso X =< 65535
              end,
              [ A0,A1,A2,A3,A4,A5,A6,A7 ]);
is_host(_) ->
    false.

%% @doc Generate host specification from binary.
-spec parse(binary()) -> { ok, host() } | { error, einval }.
parse(<<$[, _/binary>> = R) ->
    parse_ipv6_reference(R);
parse(<<Char/utf8, _/binary>> = R) when ?is_DIGIT(Char) ->
    parse_ipv4_address(R);
parse(Bin) when is_binary(Bin) ->
    case hostname_valid(Bin) of
        true ->
            { ok, { hostname, Bin } };
        false ->
            { error, einval }
    end.


%%%===================================================================
%%% Internal implementation
%%%===================================================================

%% @private
%% @doc Parse IPv6 reference
%%
%% IPv6reference  =  "[" IPv6address "]"
-spec parse_ipv6_reference(binary()) -> { ok, host() } | { error, einval }.
parse_ipv6_reference(<<$[, R/binary>>) when R =/= <<>> ->
    case binary:at(R, byte_size(R)-1) of
        $] ->
            parse_ipv6_address(binary:part(R, 0, byte_size(R)-1));
        _ ->
            { error, einval }
    end.

%% @private
%% @doc Parse IPv6 address
%%
%% IPv6reference  =  "[" IPv6address "]"
%% IPv6address    =  hexpart [ ":" IPv4address ]
%% hexpart        =  hexseq / hexseq "::" [ hexseq ] / "::" [ hexseq ]
%% hexseq         =  hex4 *( ":" hex4)
%% hex4           =  1*4HEXDIG
-spec parse_ipv6_address(binary()) -> { ok, host() } | { error, einval }.
parse_ipv6_address(Bin) when is_binary(Bin) ->
    L = binary_to_list(Bin),
    case inet:parse_address(L) of
        { ok, {_, _, _, _,   _, _, _, _ } = Addr } ->
            { ok, { ipv6, Addr } };
        _ ->
            { error, einval }
    end.

%% @private
%% @doc Parse IPv4 address
%%
%% IPv4address    =  1*3DIGIT "." 1*3DIGIT "." 1*3DIGIT "." 1*3DIGIT
-spec parse_ipv4_address(binary()) -> { ok, host() } | { error, einval }.
parse_ipv4_address(Bin) when is_binary(Bin) ->
    L = binary_to_list(Bin),
    case inet:parse_address(L) of
        { ok, {_, _, _, _} = Addr } ->
            { ok, { ipv4, Addr } };
        _ ->
            { error, einval }
    end.

%% @private
%% @doc Check that hostname agree specicification
%%
%% hostname         =  *( domainlabel "." ) toplabel [ "." ]
%%
-spec hostname_valid( binary() ) -> boolean().
hostname_valid(Bin) when is_binary(Bin) ->
    hostname_valid(binary:split(Bin, <<".">>, [ global ]));
hostname_valid([<<>>]) ->
    false;
hostname_valid([ TopLabel, <<>> ]) ->
    toplabel_valid(TopLabel, start);
hostname_valid([ TopLabel ]) ->
    toplabel_valid(TopLabel, start);
hostname_valid([ DomainLabel | Rest ]) ->
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
