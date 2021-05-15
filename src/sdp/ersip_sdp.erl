%%%
%%% Copyright (c) 2018, 2021 Dmitry Poroh
%%% All rights reserved.
%%% Distributed under the terms of the MIT License. See the LICENSE file.
%%%
%%% SDP session level
%%% RFC 4566
%%%

-module(ersip_sdp).

-export([origin/1,
         set_origin/2,
         session_name/1,
         info/1,
         uri/1,
         emails/1,
         phones/1,
         conn/1,
         set_conn/2,
         bandwidth/1,
         time/1,
         medias/1,
         set_medias/2,
         attrs/1,
         set_attrs/2,
         parse/1,
         assemble/1,
         assemble_bin/1
        ]).

-export_type([sdp/0]).

-define(crlf, "\r\n").

%%===================================================================
%% Types
%%===================================================================

-record(sdp, {origin       :: ersip_sdp_origin:origin(),
              session_name :: binary(),
              info         :: maybe_binary(),
              uri          :: maybe_binary(),
              emails       :: [binary()],
              phones       :: [binary()],
              conn         :: ersip_sdp_conn:conn() | undefined,
              bandwidth    :: ersip_sdp_bandwidth:bandwidth(),
              timings      :: ersip_sdp_time:timings(),
              key          :: maybe_binary(),
              attrs = []   :: ersip_sdp_attr:attr_list(),
              medias       :: [ersip_sdp_media:media()]
             }).
-type sdp() :: #sdp{}.
-type maybe_binary()  :: binary() | undefined.
-type parse_result(X) :: ersip_parser_aux:parse_result(X).

%%===================================================================
%% API
%%===================================================================

-spec origin(sdp()) -> ersip_sdp_origin:origin().
origin(#sdp{origin = Origin}) ->
    Origin.

-spec set_origin(ersip_sdp_origin:origin(), sdp()) -> sdp().
set_origin(Origin, #sdp{} = SDP) ->
    SDP#sdp{origin = Origin}.

-spec session_name(sdp()) -> binary().
session_name(#sdp{session_name = SessName}) ->
    SessName.

-spec info(sdp()) -> maybe_binary().
info(#sdp{info = Info}) ->
    Info.

-spec uri(sdp()) -> maybe_binary().
uri(#sdp{uri = URI}) ->
    URI.

-spec emails(sdp()) -> [binary()].
emails(#sdp{emails = Emails}) ->
    Emails.

-spec phones(sdp()) -> [binary()].
phones(#sdp{phones = Phones}) ->
    Phones.

-spec conn(sdp()) -> ersip_sdp_conn:conn() | undefined.
conn(#sdp{conn = Conn}) ->
    Conn.

-spec set_conn(ersip_sdp_conn:conn(), sdp()) -> sdp().
set_conn(Conn, #sdp{} = SDP) ->
    SDP#sdp{conn = Conn}.

-spec bandwidth(sdp()) -> ersip_sdp_bandwidth:bandwidth().
bandwidth(#sdp{bandwidth = Band}) ->
    Band.

-spec time(sdp()) -> ersip_sdp_time:timings().
time(#sdp{timings = Time}) ->
    Time.

-spec medias(sdp()) -> [ersip_sdp_media:media()].
medias(#sdp{medias = M}) ->
    M.

-spec set_medias([ersip_sdp_media:media()], sdp()) -> sdp().
set_medias(Medias, #sdp{} = SDP) ->
    SDP#sdp{medias = Medias}.

-spec attrs(sdp()) -> ersip_sdp_attr:attr_list().
attrs(#sdp{attrs = A}) ->
    A.

-spec set_attrs(ersip_sdp_attr:attr_list(), sdp()) -> sdp().
set_attrs(A, #sdp{} = SDP) ->
    SDP#sdp{attrs = A}.

%% session-description = proto-version
%%                       origin-field
%%                       session-name-field
%%                       information-field
%%                       uri-field
%%                       email-fields
%%                       phone-fields
%%                       connection-field
%%                       bandwidth-fields
%%                       time-fields
%%                       key-field
%%                       attribute-fields
%%                       media-descriptions
-spec parse(binary()) -> {ok, sdp()} | {error, term()}.
parse(Bin) ->
    Parsers = [fun parse_proto/1,
               fun ersip_sdp_origin:parse/1,
               fun parse_session_name/1,
               fun ersip_sdp_aux:parse_info/1,
               fun parse_uri/1,
               fun parse_emails/1,
               fun parse_phones/1,
               fun ersip_sdp_conn:parse/1,
               fun ersip_sdp_bandwidth:parse/1,
               fun ersip_sdp_time:parse/1,
               fun ersip_sdp_aux:parse_key/1,
               fun ersip_sdp_attr:parse/1,
               fun ersip_sdp_media:parse/1
              ],
    case ersip_parser_aux:parse_all(Bin, Parsers) of
        {ok, [_Proto, Origin, SessionName,
              Info, URI, Emails, Phones,
              Conn, Band, Time, Key,
              Attrs, Medias], <<>>} ->
            SDP = #sdp{origin       = Origin,
                       session_name = SessionName,
                       info         = Info,
                       uri          = URI,
                       emails       = Emails,
                       phones       = Phones,
                       conn         = Conn,
                       bandwidth    = Band,
                       timings      = Time,
                       key          = Key,
                       attrs        = Attrs,
                       medias       = Medias
                      },
            {ok, SDP};
        {ok, _, Rest} ->
            {error, {invalid_sdp, Rest}};
        {error, Reason} ->
            {error, {invalid_sdp, Reason}}
    end.

-spec assemble(sdp()) -> iolist().
assemble(#sdp{} = SDP) ->
    #sdp{origin       = Origin,
         session_name = SessionName,
         info         = Info,
         uri          = URI,
         emails       = Emails,
         phones       = Phones,
         conn         = Conn,
         bandwidth    = Band,
         timings      = Time,
         key          = Key,
         attrs        = Attrs,
         medias       = Medias
        } = SDP,
    [<<"v=0", ?crlf>>,
     ersip_sdp_origin:assemble(Origin),
     <<"s=">>, SessionName, ?crlf,
     ersip_sdp_aux:assemble_info(Info),
     assemble_uri(URI),
     assemble_emails(Emails),
     assemble_phones(Phones),
     ersip_sdp_conn:assemble(Conn),
     ersip_sdp_bandwidth:assemble(Band),
     ersip_sdp_time:assemble(Time),
     ersip_sdp_aux:assemble_key(Key),
     ersip_sdp_attr:assemble(Attrs),
     ersip_sdp_media:assemble(Medias)].

-spec assemble_bin(sdp()) -> binary().
assemble_bin(#sdp{} = SDP) ->
    iolist_to_binary(assemble(SDP)).

%%%===================================================================
%%% Internal implementation
%%%===================================================================

%% proto-version =       %x76 "=" 1*DIGIT CRLF
%%                       ;this memo describes version 0
-spec parse_proto(binary()) -> parse_result(0).
parse_proto(<<"v=0" ?crlf, Rest/binary>>) ->
    {ok, 0, Rest};
parse_proto(<<"v=", C/utf8, ?crlf, _/binary>>) ->
    {error, {unsupported_version, <<C/utf8>>}};
parse_proto(Bin) ->
    unexpected_attribute_error(version, Bin).

%% session-name-field =  %x73 "=" text CRLF
-spec parse_session_name(binary()) -> parse_result(binary()).
parse_session_name(<<"s=", Rest/binary>>) ->
    ersip_sdp_aux:binary_to_eol(session_name, Rest);
parse_session_name(Bin) ->
    unexpected_attribute_error(session_name, Bin).

%% uri-field =           [%x75 "=" uri CRLF]
-spec parse_uri(binary()) -> parse_result(maybe_binary()).
parse_uri(<<"u=", Rest/binary>>) ->
    ersip_sdp_aux:binary_to_eol(uri, Rest);
parse_uri(Bin) ->
    {ok, undefined, Bin}.

-spec assemble_uri(maybe_binary()) -> iolist().
assemble_uri(undefined) ->
    [];
assemble_uri(URI) ->
    [<<"u=">>, URI, ?crlf].

%% email-fields =        *(%x65 "=" email-address CRLF)
-spec parse_emails(binary()) -> parse_result([binary()]).
parse_emails(Bin) ->
    do_parse_emails(Bin, []).

-spec assemble_emails([binary()]) -> iolist().
assemble_emails(Emails) ->
    [[<<"e=">>, E, ?crlf] || E <- Emails].

%% phone-fields =        *(%x70 "=" phone-number CRLF)
-spec parse_phones(binary()) -> parse_result([binary()]).
parse_phones(Bin) ->
    do_parse_phones(Bin, []).

-spec assemble_phones([binary()]) -> iolist().
assemble_phones(Phones) ->
    [[<<"p=">>, P, ?crlf] || P <- Phones].

-spec unexpected_attribute_error(atom(), binary()) -> {error, term()}.
unexpected_attribute_error(Expected, Bin) ->
    ersip_sdp_aux:unexpected_attribute_error(Expected, Bin).

-spec do_parse_emails(binary(), [binary()]) -> parse_result([binary()]).
do_parse_emails(<<"e=", Rest/binary>>, Acc) ->
    case ersip_sdp_aux:binary_to_eol(email, Rest) of
        {ok, V, Rest1} ->
            do_parse_emails(Rest1, [V | Acc]);
        {error, _} = Error ->
            Error
    end;
do_parse_emails(Bin, Acc) ->
    {ok, lists:reverse(Acc), Bin}.

-spec do_parse_phones(binary(), [binary()]) -> parse_result([binary()]).
do_parse_phones(<<"p=", Rest/binary>>, Acc) ->
    case ersip_sdp_aux:binary_to_eol(phone, Rest) of
        {ok, V, Rest1} ->
            do_parse_phones(Rest1, [V | Acc]);
        {error, _} = Error ->
            Error
    end;
do_parse_phones(Bin, Acc) ->
    {ok, lists:reverse(Acc), Bin}.
