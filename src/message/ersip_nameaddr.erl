%%%
%%% Copyright (c) 2017, 2020 Dmitry Poroh
%%% All rights reserved.
%%% Distributed under the terms of the MIT License. See the LICENSE file.
%%%
%%% Name and address (Used in From/To/Contact etc. fields)
%%%

-module(ersip_nameaddr).

-export([parse/1,
         parse/2,
         assemble/2,
         assemble_display_name/1,
         assemble_display_name_bin/1
        ]).
-export_type([display_name/0]).

-include("ersip_sip_abnf.hrl").

%%===================================================================
%% Types
%%===================================================================

-type display_name() :: ersip_display_name:display_name().

%%===================================================================
%% API
%%===================================================================

%% [display-name] LAQUOT addr-spec RAQUOT
%% display-name   =  *(token LWS)/ quoted-string
-spec parse(binary()) ->  ersip_parser_aux:parse_result({display_name(), ersip_uri:uri()}).
parse(NameAddrBin) ->
    parse(NameAddrBin, [<<",">>, <<";">>, <<" ">>, <<"\t">>]).

-spec parse(binary(), [binary()]) ->  ersip_parser_aux:parse_result({display_name(), ersip_uri:uri()}).
parse(NameAddrBin, AddrSpecSeps) ->
    NA = ersip_bin:trim_head_lws(NameAddrBin),
    EmptyDN = ersip_display_name:empty(),
    case parse_display_name(NA) of
        {DisplayName, <<"<", R/binary>>} ->
            case binary:match(R, <<">">>) of
                {Pos, 1} ->
                    <<Addr:Pos/binary, $>, Rest/binary>> = R,
                    case ersip_uri:parse(Addr) of
                        {ok, URI} ->
                            {ok, {DisplayName, URI}, Rest};
                        {error, URIErr} ->
                            {error, {invalid_nameaddr, {uri_error, URIErr}}}
                    end;
                _ ->
                    {error, {invalid_nameaddr, {noraquot, NameAddrBin}}}
            end;
        {EmptyDN, <<R/binary>>} ->
            case binary:match(R, AddrSpecSeps) of
                {Pos, 1} ->
                    <<Addr:Pos/binary, Rest/binary>> = R,
                    case ersip_uri:parse(ersip_bin:trim_lws(Addr)) of
                        {ok, URI} ->
                            {ok, {EmptyDN, URI}, Rest};
                        {error, URIErr} ->
                            {error, {invalid_nameaddr, {uri_error, URIErr}}}
                    end;
                _ ->
                    case ersip_uri:parse(R) of
                        {ok, URI} ->
                            {ok, {EmptyDN, URI}, <<>>};
                        {error, URIErr} ->
                            {error, {invalid_nameaddr, {uri_error, URIErr}}}
                    end
            end;
        error ->
            {error, {invalid_nameaddr, NameAddrBin}}
    end.

-spec assemble(display_name(), ersip_uri:uri()) -> iolist().
assemble(DisplayName, URI) ->
    DN = ersip_display_name:assemble(DisplayName),
    [DN,
     case ersip_iolist:is_empty(DN) of
         true  -> <<"">>;
         false -> <<" ">>
     end,
     $<, ersip_uri:assemble(URI), $>
    ].

%% @deprecated
%% @doc Assemble disloay name to iolist
%%
%% Please use ersip_display_name:assemble/1.
-spec assemble_display_name(display_name()) -> iolist().
assemble_display_name(DN) ->
    ersip_display_name:assemble(DN).

%% @deprecated
%% @doc Assemble disloay name to iolist
%%
%% Please use ersip_display_name:assemble_bin/1.
-spec assemble_display_name_bin(display_name()) -> binary().
assemble_display_name_bin(DN) ->
    ersip_display_name:assemble_bin(DN).

%%===================================================================
%% Internal implementation
%%===================================================================

%% display-name   =  *(token LWS)/ quoted-string
-spec parse_display_name(binary()) -> Result when
      Result :: {display_name(), Rest :: binary()}
              | error.
parse_display_name(<<Char/utf8, _/binary>> = DNBin) when ?is_token_char(Char); Char == $" ->
    case ersip_display_name:parse_dn(DNBin) of
        {ok, DN, Rest} ->
            case ersip_bin:trim_head_lws(Rest) of
                <<"<", _/binary>> = Addr -> {DN, Addr};
                _ -> {ersip_display_name:empty(), DNBin}
            end;
        {error, _} -> error
    end;
parse_display_name(<<"<", _/binary>> = Addr) ->
    {ersip_display_name:empty(), Addr};
parse_display_name(_) ->
    error.

