%%
%% Copyright (c) 2020 Dmitry Poroh
%% All rights reserved.
%% Distributed under the terms of the MIT License. See the LICENSE file.
%%
%% SDP media attribute rtpmap
%%

-module(ersip_sdp_attr_fmtp).

-export([new/2,
    format/1,
    set_fmt/2,
    params/1,
    set_params/2,
    parse/1,
    assemble/1,
    assemble_bin/1
]).

%%%===================================================================
%%% Types
%%%===================================================================

-record(fmtp, {
    fmt               :: format(),
    params            :: fmt_params()
}).


-type fmtp()          :: #fmtp{}.
-type format()        :: pos_integer().
-type fmt_params()    :: undefined | binary().
-type parse_result()  :: ersip_parser_aux:parse_result(fmtp()).
-type parse_result(X) :: ersip_parser_aux:parse_result(X).

-export_type([
    format/0,
    fmt_params/0
]).

%%%===================================================================
%%% API
%%%===================================================================

-spec new(format(), fmt_params()) -> fmtp().
new(Fmt, FormatParams) ->
    #fmtp{
        fmt = Fmt,
        params = FormatParams
    }.

-spec format(fmtp()) -> format().
format(#fmtp{fmt = Fmt}) ->
    Fmt.

-spec params(fmtp()) -> fmt_params().
params(#fmtp{params = Params}) ->
    Params.

-spec set_fmt(format(), fmtp()) -> fmtp().
set_fmt(Format, Fmtp) ->
    Fmtp#fmtp{fmt = Format}.

-spec set_params(fmt_params(), fmtp()) -> fmtp().
set_params(Params, Fmtp) ->
    Fmtp#fmtp{params = Params}.

-spec parse(binary()) -> parse_result().
parse(Bin) ->
    do_parse_fmtp(Bin).

-spec assemble(fmtp()) -> iolist().
assemble(Fmtp) ->
    assemble_fmtp(Fmtp).

-spec assemble_bin(fmtp()) -> binary().
assemble_bin(Fmtp) ->
    iolist_to_binary(assemble(Fmtp)).

%%%===================================================================
%%% Internal implementation
%%%===================================================================

-define(sp, " ").

%% https://datatracker.ietf.org/doc/html/rfc8866#section-6.6
%%
%% Syntax:
%%
%%         fmtp-value = fmt SP format-specific-params
%%         format-specific-params = byte-string
%%           ; Notes:
%%           ; - The format parameters are media type parameters and
%%           ;   need to reflect their syntax.
%%
%%   Example:
%%
%%         a=fmtp:96 profile-level-id=42e016;max-mbps=108000;max-fs=3600
%%

-spec do_parse_fmtp(binary()) -> parse_result().
do_parse_fmtp(Bin) ->
    Parsers = [
        fun ersip_parser_aux:parse_non_neg_int/1,
        fun ersip_parser_aux:trim_lws/1,
        fun binary_string/1
    ],
    case ersip_parser_aux:parse_all(Bin, Parsers) of
        {ok, Result, Rest} ->
            [Type, _, BString] = Result,
            {ok, #fmtp{
                fmt = Type,
                params = BString
            }, Rest};
        {error, Reason} ->
            {error, {invalid_fmtp, Reason}}
    end.


-spec binary_string(binary()) -> parse_result(binary()).
binary_string(<<>>) ->
    {error, {invalid_binary_string, empty}};

binary_string(<<Rest/binary>>) ->
    {ok, Rest, <<>>}.


-spec assemble_fmtp(fmtp()) -> iolist().
assemble_fmtp(#fmtp{} = Fmtp) ->
    [
        integer_to_binary(format(Fmtp)),
        encoding_params_to_iolist(params(Fmtp))
    ].


-spec encoding_params_to_iolist(fmt_params()) -> iolist().
encoding_params_to_iolist(undefined) -> [];
encoding_params_to_iolist(B) -> [?sp, B].