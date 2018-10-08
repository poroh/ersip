%%
%% Copyright (c) 2017 Dmitry Poroh
%% All rights reserved.
%% Distributed under the terms of the MIT License. See the LICENSE file.
%%
%% Name and address (Used in From/To/Contact etc. fields)
%%

-module(ersip_nameaddr).

-export([parse/1,
         assemble/2
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
-spec parse(binary()) -> Result when
      Result :: {ok, {display_name(),
                      ersip_uri:uri()
                     },
                 Rest :: binary()
                }
              | {error, {einval, atom()}}.
parse(NameAddrBin) ->
    NA = ersip_bin:trim_head_lws(NameAddrBin),
    case parse_display_name(NA) of
        {DisplayName, <<"<", R/binary>>} ->
            case binary:match(R, <<">">>) of
                {Pos, 1} ->
                    Addr = binary:part(R, 0, Pos),
                    RestLen = byte_size(R) - byte_size(Addr) - 1,
                    case ersip_uri:parse(Addr) of
                        {ok, URI} ->
                            {ok, {DisplayName, URI}, binary:part(R, Pos+1, RestLen)};
                        Error ->
                            Error
                    end;
                _ ->
                    {error, {einval, address}}
            end;
        {{display_name, []} = DN, <<R/binary>>} ->
            case binary:match(R, [<<",">>, <<";">>]) of
                {Pos, 1} ->
                    Addr = binary:part(R, 0, Pos),
                    case ersip_uri:parse(ersip_bin:trim_lws(Addr)) of
                        {ok, URI} ->
                            RestLen = byte_size(R) - byte_size(Addr),
                            {ok, {DN, URI}, binary:part(R, Pos, RestLen)};
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

assemble_display_name({display_name, L}) when is_list(L) ->
    ersip_iolist:join(<<" ">>, L);
assemble_display_name({display_name, V}) when is_binary(V) ->
    V.
