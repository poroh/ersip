%%
%% Copyright (c) 2018 Dmitry Poroh
%% All rights reserved.
%% Distributed under the terms of the MIT License. See the LICENSE file.
%%
%% SDP origin
%%

-module(ersip_sdp_origin).

-export([username/1,
         session_id/1,
         session_version/1,
         address/1,
         parse/1,
         assemble/1
        ]).

-export_type([origin/0]).

%%%===================================================================
%%% Types
%%%===================================================================

-record(origin, {username     :: binary(),
                 sess_id      :: non_neg_integer(),
                 sess_version :: non_neg_integer(),
                 address      :: ersip_sdp_addr:addr()
                }).
-type origin() :: #origin{}.

%%%===================================================================
%%% API
%%%===================================================================

-spec username(origin()) -> binary().
username(#origin{username = U}) ->
    U.

-spec session_id(origin()) -> non_neg_integer().
session_id(#origin{sess_id = Id}) ->
    Id.

-spec session_version(origin()) -> non_neg_integer().
session_version(#origin{sess_version = Ver}) ->
    Ver.

-spec address(origin()) -> ersip_sdp_addr:addr().
address(#origin{address = Addr}) ->
    Addr.

-define(crlf, "\r\n").

%%  origin-field =        %x6f "=" username SP sess-id SP sess-version SP
%%                         nettype SP addrtype SP unicast-address CRLF
-spec parse(binary()) -> ersip_parser_aux:parse_result(origin()).
parse(<<"o=", Rest/binary>>) ->
    case binary:split(Rest, <<?crlf>>) of
        [_] ->
            {error, {invalid_origin, {no_crlf, Rest}}};
        [OriginBin, Rest1] ->
            case binary:split(OriginBin, <<" ">>, [global]) of
                [UserName, SessIdBin, SessVerBin,
                 NetType, AddrType, UnicastAddr] ->
                    case build_origin(UserName, SessIdBin, SessVerBin,
                                      NetType, AddrType, UnicastAddr) of
                        {ok, Origin} ->
                            {ok, Origin, Rest1};
                        {error, _} = Error ->
                            Error
                    end;
                _ ->
                    {error, {invalid_origin, OriginBin}}
            end
    end;
parse(V) ->
    ersip_sdp_aux:unexpected_attribute_error(origin, V).

-spec assemble(origin()) -> iolist().
assemble(#origin{} = Origin) ->
    #origin{username     = UserName,
            sess_id      = SessId,
            sess_version = SessVersion,
            address      = Addr} = Origin,
    [<<"o=">>, UserName, <<" ">>, integer_to_binary(SessId),
     <<" ">>, integer_to_binary(SessVersion), <<" ">>,
     ersip_sdp_addr:assemble(Addr), <<"\r\n">>].

%%%===================================================================
%%% Internal implementation
%%%===================================================================

-spec build_origin(binary(), binary(), binary(),
                   binary(), binary(), binary()) -> Result when
      Result :: {ok, origin()}
              | {error, term()}.
build_origin(UserName, SessIdBin, SessVerBin,
             NetType, AddrType, UnicastAddr) ->
    UnicastAddrParser =
        fun(AddrBin) ->
                case ersip_sdp_addr:parse(NetType, AddrType, AddrBin) of
                    {ok, Addr} ->
                        {ok, Addr, <<>>};
                    {error, _} = Error ->
                        Error
                end
        end,
    Parsers = [{#origin.sess_id,       sess_id,      fun ersip_parser_aux:parse_non_neg_int/1, SessIdBin},
               {#origin.sess_version,  sess_version, fun ersip_parser_aux:parse_non_neg_int/1, SessVerBin},
               {#origin.address,       address,      UnicastAddrParser,                        UnicastAddr}],
    Origin = #origin{username = UserName,
                     sess_id = 0, sess_version = 0,
                     address = {ip4, {127, 0, 0, 1}}},
    lists:foldl(fun({Elem, Name, Parser, BinVal}, {ok, OriginAcc}) ->
                        case Parser(BinVal) of
                            {ok, Val, <<>>} ->
                                {ok, erlang:setelement(Elem, OriginAcc, Val)};
                            {ok, _, _} ->
                                {error, {invalid_origin, {Name, BinVal}}};
                            {error, Reason} ->
                                {error, {invalid_origin, {Name, Reason}}}
                        end;
                   (_, {error, _} = Error) ->
                        Error
                end,
                {ok, Origin},
                Parsers).
