%%
%% Copyright (c) 2018 Dmitry Poroh
%% All rights reserved.
%% Distributed under the terms of the MIT License. See the LICENSE file.
%%
%% SDP media layers
%%

-module(ersip_sdp_media).

-export([type/1,
         port/1,
         port_num/1,
         protocol/1,
         formats/1,
         set_formats/2,
         conn/1,
         set_conn/2,
         attrs/1,
         set_attrs/2,
         parse/1,
         assemble/1
        ]).

-export_type([media/0]).

%%%===================================================================
%%% Types
%%%===================================================================

-record(media, {type       :: media_type(),                      %% m=
                port       :: inet:port_number(),                %% m=
                port_num   :: non_neg_integer() | undefined,     %% m=
                protocol   :: nonempty_list(binary()),           %% m=
                fmts       :: [binary()],                        %% m=
                title      :: maybe_binary(),                    %% i=
                conn       :: ersip_sdp_conn:conn() | undefined, %% c=
                bandwidth  :: ersip_sdp_bandwidth:bandwidth(),   %% b=
                key        :: maybe_binary(),                    %% k=
                attrs = [] :: ersip_sdp_attr:attr_list()
               }).

-type media() :: #media{}.

-type media_type()    :: binary().
-type maybe_binary()  :: binary() | undefined.

-type parse_result()  :: ersip_parser_aux:parse_result([media()]).
-type parse_result(X) :: ersip_parser_aux:parse_result(X).

%%%===================================================================
%%% API
%%%===================================================================

-spec type(media()) -> binary().
type(#media{type = T}) ->
    T.

-spec port(media()) -> inet:port_number().
port(#media{port = P}) ->
    P.

-spec port_num(media()) -> non_neg_integer() | undefined.
port_num(#media{port_num = PN}) ->
    PN.

-spec protocol(media()) -> nonempty_list(binary()).
protocol(#media{protocol = P}) ->
    P.

-spec formats(media()) -> [binary()].
formats(#media{fmts = FMTS}) ->
    FMTS.

-spec set_formats([binary()], media()) -> media().
set_formats(FMTS, #media{} = Media) ->
     Media#media{fmts = FMTS}.    

-spec conn(media()) -> ersip_sdp_conn:conn() | undefined.
conn(#media{conn = Conn}) ->
    Conn.

-spec set_conn(ersip_sdp_conn:conn(), media()) -> media().
set_conn(Conn, #media{} = Media) ->
    Media#media{conn = Conn}.

-spec attrs(media()) -> ersip_sdp_attr:attr_list().
attrs(#media{attrs = A}) ->
    A.

-spec set_attrs(ersip_sdp_attr:attr_list(), media()) -> media().
set_attrs(A, #media{} = Media) ->
    Media#media{attrs = A}.

-spec parse(binary()) -> parse_result().
parse(Bin) ->
    do_parse_medias(Bin, []).

-spec assemble([media()]) -> iolist().
assemble(Medias) ->
    [assemble_media(M) || M <- Medias].

%%%===================================================================
%%% Internal implementation
%%%===================================================================

-define(crlf, "\r\n").

%%  %x6d "=" media SP port ["/" integer] SP proto 1*(SP fmt) CRLF
-spec do_parse_medias(binary(), [media()]) -> parse_result().
do_parse_medias(<<"m=", Rest/binary>>, Acc) ->
    Parsers = [fun ersip_sdp_aux:parse_token/1,
               fun ersip_parser_aux:parse_lws/1,
               fun ersip_parser_aux:parse_non_neg_int/1,
               fun parse_num_ports/1,
               fun ersip_parser_aux:parse_lws/1,
               fun parse_proto/1,
               fun parse_fmts/1,
               fun ersip_sdp_aux:parse_crlf/1,
               fun ersip_sdp_aux:parse_info/1,
               fun ersip_sdp_conn:parse/1,
               fun ersip_sdp_bandwidth:parse/1,
               fun ersip_sdp_aux:parse_key/1,
               fun ersip_sdp_attr:parse/1],
    case ersip_parser_aux:parse_all(Rest, Parsers) of
        {ok, Result, Rest1} ->
            [MediaType, _, Port, NumPorts, _, Protocol, Formats, _,
             Title,
             Conn,
             Bandwidth,
             Key,
             Attrs] = Result,
            Media = #media{type      = MediaType,
                           port      = Port,
                           port_num  = NumPorts,
                           protocol  = Protocol,
                           fmts      = Formats,
                           title     = Title,
                           conn      = Conn,
                           bandwidth = Bandwidth,
                           key       = Key,
                           attrs     = Attrs},
            do_parse_medias(Rest1, [Media | Acc]);
        {error, Reason} ->
            {error, {invalid_media, Reason}}
    end;
do_parse_medias(Rest, Acc) ->
    {ok, lists:reverse(Acc), Rest}.

-spec assemble_media(media()) -> iolist().
assemble_media(#media{} = Media) ->
    #media{type      = MediaType,
           port      = Port,
           port_num  = NumPorts,
           protocol  = Protocol,
           fmts      = Formats,
           title     = Title,
           conn      = Conn,
           bandwidth = Bandwidth,
           key       = Key,
           attrs     = Attrs} = Media,
    [<<"m=">>, MediaType, <<" ">>, integer_to_binary(Port),
     case NumPorts of
         1 -> [];
         _ -> [<<"/">>, integer_to_binary(NumPorts)]
     end,
     <<" ">>, ersip_iolist:join(<<"/">>, Protocol),
     [[<<" ">>, Fmt] || Fmt <- Formats],
     <<"\r\n">>,
     ersip_sdp_aux:assemble_info(Title),
     ersip_sdp_conn:assemble(Conn),
     ersip_sdp_bandwidth:assemble(Bandwidth),
     ersip_sdp_aux:assemble_key(Key),
     ersip_sdp_attr:assemble(Attrs)
    ].

%% ["/" integer]
-spec parse_num_ports(binary()) -> parse_result(non_neg_integer()).
parse_num_ports(<<"/", Rest/binary>>) ->
    case ersip_parser_aux:parse_non_neg_int(Rest) of
        {ok, _, _} = Ok ->
            Ok;
        {error, Reason} ->
            {error, {bad_num_ports, Reason}}
    end;
parse_num_ports(Other) ->
    {ok, 1, Other}.

-spec parse_proto(binary()) -> parse_result(nonempty_list(binary())).
parse_proto(Bin) ->
    case ersip_parser_aux:parse_token(Bin) of
        {ok, Token, Rest} ->
            do_parse_proto(Rest, [Token]);
        {error, Reason} ->
            {error, {invalid_protocol, Reason}}
    end.

-spec do_parse_proto(binary(), nonempty_list(binary())) -> parse_result(nonempty_list(binary())).
do_parse_proto(<<"/", Rest/binary>>, Acc) ->
    case ersip_sdp_aux:parse_token(Rest) of
        {ok, Token, Rest1} ->
            do_parse_proto(Rest1, [Token | Acc]);
        {error, Reason} ->
            {error, {invalid_protocol, Reason}}
    end;
do_parse_proto(Rest, Acc) ->
    {ok, lists:reverse(Acc), Rest}.

%% 1*(SP fmt)
-spec parse_fmts(binary()) -> parse_result([binary()]).
parse_fmts(Bin) ->
    do_parse_fmts(Bin, []).

-spec do_parse_fmts(binary(), [binary()]) -> parse_result([binary()]).
do_parse_fmts(<<" ", Rest/binary>>, Acc) ->
    case ersip_sdp_aux:parse_token(Rest) of
        {ok, Fmt, Rest1} ->
            do_parse_fmts(Rest1, [Fmt | Acc]);
        {error, Reason} ->
            {error, {invalid_format, Reason}}
    end;
do_parse_fmts(Bin, Acc) ->
    case Acc of
        [] ->
            {error, at_least_one_format_required};
        _ ->
            {ok, lists:reverse(Acc), Bin}
    end.

