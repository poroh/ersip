%%
%% Copyright (c) 2017 Dmitry Poroh
%% All rights reserved.
%% Distributed under the terms of the MIT License. See the LICENSE file.
%%
%% Name and address (Used in From/To/Contact etc. fields)
%%

-module(ersip_nameaddr).

-export([parse/1,
         parse/2,
         assemble/2,
         assemble_display_name/1,
         assemble_display_name_bin/1
        ]).
-export_type([display_name/0]).

-include("ersip_sip_abnf.hrl").

%%%===================================================================
%%% Types
%%%===================================================================

-type display_name() :: {display_name, binary() | [binary()]}.

%%%===================================================================
%%% API
%%%===================================================================

%% [display-name] LAQUOT addr-spec RAQUOT
%% display-name   =  *(token LWS)/ quoted-string
-spec parse(binary()) ->  ersip_parser_aux:parse_result({display_name(), ersip_uri:uri()}).
parse(NameAddrBin) ->
    parse(NameAddrBin, [<<",">>, <<";">>, <<" ">>, <<"\t">>]).

-spec parse(binary(), [binary()]) ->  ersip_parser_aux:parse_result({display_name(), ersip_uri:uri()}).
parse(NameAddrBin, AddrSpecSeps) ->
    NA = ersip_bin:trim_head_lws(NameAddrBin),
    case parse_display_name(NA) of
        {_, <<>>} ->
            {error, {einval, no_uri}};
        {DisplayName, <<"<", R/binary>>} ->
            case binary:match(R, <<">">>) of
                {Pos, 1} ->
                    <<Addr:Pos/binary, $>, Rest/binary>> = R,
                    case ersip_uri:parse(Addr) of
                        {ok, URI} ->
                            {ok, {DisplayName, URI}, Rest};
                        Error ->
                            Error
                    end;
                _ ->
                    {error, {einval, address}}
            end;
        {{display_name, []} = DN, <<R/binary>>} ->
            case binary:match(R, AddrSpecSeps) of
                {Pos, 1} ->
                    <<Addr:Pos/binary, Rest/binary>> = R,
                    case ersip_uri:parse(ersip_bin:trim_lws(Addr)) of
                        {ok, URI} ->
                            {ok, {DN, URI}, Rest};
                        Error ->
                            Error
                    end;
                _ ->
                    case ersip_uri:parse(R) of
                        {ok, URI} ->
                            {ok, {DN, URI}, <<>>};
                        Error ->
                            Error
                    end
            end;
        error ->
            {error, {einval, address}}
    end.

-spec assemble(display_name(), ersip_uri:uri()) -> iolist().
assemble(DisplayName, URI) ->
    DN = assemble_display_name(DisplayName),
    [DN,
     case ersip_iolist:is_empty(DN) of
         true ->
             <<"">>;
         false ->
             <<" ">>
     end,
     $<, ersip_uri:assemble(URI), $>
    ].

-spec assemble_display_name(display_name()) -> iolist().
assemble_display_name({display_name, L}) when is_list(L) ->
    ersip_iolist:join(<<" ">>, L);
assemble_display_name({display_name, V}) when is_binary(V) ->
    V.

-spec assemble_display_name_bin(display_name()) -> binary().
assemble_display_name_bin({display_name, _} = DN) ->
    iolist_to_binary(assemble_display_name(DN)).


%%%===================================================================
%%% Internal implementation
%%%===================================================================

%% display-name   =  *(token LWS)/ quoted-string
-spec parse_display_name(binary()) -> Result when
      Result :: {display_name(), Rest :: binary()}
              | error.
parse_display_name(<<>>) ->
    error;
parse_display_name(<<$", _/binary>> = Quoted) ->
    case ersip_parser_aux:quoted_string(Quoted) of
        {ok, Q, Rest} ->
            {{display_name, Q}, ersip_bin:trim_head_lws(Rest)};
        error ->
            error
    end;
parse_display_name(<<Char/utf8, _/binary>> = TL) when ?is_token_char(Char) ->
    case ersip_parser_aux:token_list(TL, lws) of
        {ok,  Tokens, Rest} ->
            case Rest of
                <<"<", _/binary>> ->
                    {{display_name,  Tokens}, Rest};
                _ ->
                    {{display_name, []}, TL}
            end;
        error ->
            {{display_name, []}, TL}
    end;
parse_display_name(<<"<", _/binary>> = Addr) ->
    {{display_name, []}, Addr};
parse_display_name(_) ->
    error.

