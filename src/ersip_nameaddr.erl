%%
%% Copyright (c) 2017 Dmitry Poroh
%% All rights reserved.
%% Distributed under the terms of the MIT License. See the LICENSE file.
%%
%% Name and address (Used in From/To/Contact etc. fields)
%%

-module(ersip_nameaddr).

-export([parse/1]).
-include("ersip_sip_abnf.hrl").

%%%===================================================================
%%% API
%%%===================================================================

%% [ display-name ] LAQUOT addr-spec RAQUOT
%% display-name   =  *(token LWS)/ quoted-string
-spec parse(binary()) -> Result when
      Result :: { ok, { DisplayName :: { display_name, binary() | [ binary() ]},
                        Addr :: ersip_uri:uri()
                      },
                  Rest :: binary()
                }
              | { error, { einval, atom() } }.
parse(NameAddrBin) ->
    NA = ersip_bin:trim_head_lws(NameAddrBin),
    case parse_display_name(NA) of
        { DisplayName, <<"<", R/binary>> } ->
            case binary:match(R, <<">">>) of
                { Pos, 1 } ->
                    Addr = binary:part(R, 0, Pos),
                    RestLen = byte_size(R) - byte_size(Addr) - 1,
                    case ersip_uri:parse(Addr) of
                        { ok, URI } ->
                            { ok, { DisplayName, URI}, binary:part(R, Pos+1, RestLen) };
                        Error ->
                            Error
                    end;
                _ ->
                    { error, { einval, address } }
            end;
        error ->
            { error, {einval, address} }
    end.

%%%===================================================================
%%% Internal implementation
%%%===================================================================

%% display-name   =  *(token LWS)/ quoted-string
-spec parse_display_name(binary()) -> Result when
      Result :: { { display_name, DisplayName :: binary() | [ binary() ] },
                  Rest :: binary() }
              | error.
parse_display_name(<<>>) ->
    error;
parse_display_name(<<$", _/binary>> = Quoted) ->
    case ersip_parser_aux:quoted_string(Quoted) of
        { ok, Q, Rest } ->
            {  { display_name, Q }, ersip_bin:trim_head_lws(Rest) };
        error ->
            error
    end;
parse_display_name(<<Char/utf8, _/binary>> = TL) when ?is_token_char(Char) ->
    case ersip_parser_aux:token_list(TL, lws) of
        { ok,  Tokens, Rest } ->
            { { display_name,  Tokens }, Rest };
        error ->
            error
    end;
parse_display_name(<<"<", _/binary>> = Addr) ->
    { { display_name, [] }, Addr }.
