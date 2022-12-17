%%
%% Copyright (c) 2020 Dmitry Poroh
%% All rights reserved.
%% Distributed under the terms of the MIT License. See the LICENSE file.
%%
%% SDP media ssrc attribute
%%

-module(ersip_sdp_attr_ssrc).

-export([id/1,
         set_id/2,
         attr/1,
         set_attr/2,
         parse/1,
         assemble/1,
         assemble_bin/1
        ]).

-export_type([ssrc/0]).

%%%===================================================================
%%% Types
%%%===================================================================

-record(ssrc, {
    id                :: ssrc_id(),
    attr              :: undefined | ersip_sdp_attr:attr()
}).

-type ssrc()          :: #ssrc{}.
-type ssrc_id()       :: non_neg_integer().
-type parse_result()  :: ersip_parser_aux:parse_result(ssrc()).

%%%===================================================================
%%% API
%%%===================================================================

-spec id(ssrc()) -> ssrc_id().
id(#ssrc{id = I}) ->
    I.

-spec attr(ssrc()) -> ersip_sdp_attr:attr().
attr(#ssrc{attr = A}) ->
    A.

-spec set_id(ssrc_id(), ssrc()) -> ssrc().
set_id(I, Ssrc) ->
    Ssrc#ssrc{id = I}.

-spec set_attr(ersip_sdp_attr:attr(), ssrc()) -> ssrc().
set_attr(A, Ssrc) ->
    Ssrc#ssrc{attr = A}.


-spec parse(binary()) -> parse_result().
parse(Bin) ->
    do_parse_ssrc(Bin).

-spec assemble(ssrc()) -> iolist().
assemble(Ssrc) ->
    assemble_ssrc(Ssrc).

-spec assemble_bin(ssrc()) -> binary().
assemble_bin(Ssrc) ->
    iolist_to_binary(assemble(Ssrc)).

%%%===================================================================
%%% Internal implementation
%%%===================================================================

-define(sp, " ").

%% https://datatracker.ietf.org/doc/html/rfc5576#page-5
%%
%%   ssrc-attr = "ssrc:" ssrc-id SP attribute
%%   ; The base definition of "attribute" is in RFC 4566.
%%   ; (It is the content of "a=" lines.)
%%
%%   ssrc-id = integer ; 0 .. 2**32 - 1
%%
%%   attribute =/ ssrc-attr
%%
-spec do_parse_ssrc(binary()) -> parse_result().
do_parse_ssrc(<<Rest/binary>>) ->
    Parsers = [fun ersip_parser_aux:parse_non_neg_int/1,  %% ssrc-id
               fun ersip_parser_aux:parse_lws/1,
               fun ersip_sdp_attr:parse_attr/1
              ],
    case ersip_parser_aux:parse_all(Rest, Parsers) of
        {ok, Result, _} ->
            [Id, _, Attr] = Result,
            R1 = #ssrc{
                id = Id,
                attr = Attr
            },
            {ok, R1, <<>>};
        {error, Reason} ->
            {error, {invalid_ssrc, Reason}}
    end.

-spec assemble_ssrc(ssrc()) -> iolist().
assemble_ssrc(#ssrc{} = Ssrc) ->
    [to_iolist_item(Ssrc#ssrc.id),
     attr_to_iolist(Ssrc)].

-spec attr_to_iolist(ssrc()) -> iolist().
attr_to_iolist(#ssrc{attr = undefined}) ->
    [];
attr_to_iolist(#ssrc{attr = {N, V}}) ->
    [?sp, to_iolist_item(N), $:, to_iolist_item(V)];
attr_to_iolist(#ssrc{attr = N}) ->
    [?sp, to_iolist_item(N)].

-spec to_iolist_item(term()) -> iolist().
to_iolist_item(N) when is_number(N) -> [integer_to_binary(N)];
to_iolist_item(O) -> [O].
