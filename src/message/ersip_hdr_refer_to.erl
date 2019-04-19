%%%
%%% Copyright (c) 2019 Dmitry Poroh
%%% All rights reserved.
%%% Distributed under the terms of the MIT License. See the LICENSE file.
%%%
%%% SIP Refer-To header
%%%
%%% RFC 3515
%%%

-module(ersip_hdr_refer_to).

-export([new/1,
         uri/1,
         set_uri/2,
         display_name/1,
         set_display_name/2,
         all_raw_params/1,
         parse/1,
         make/1,
         assemble/1,
         assemble_bin/1,
         build/2]).

-export_type([refer_to/0]).

%%===================================================================
%% Types
%%===================================================================

-record(refer_to, {display_name :: ersip_nameaddr:display_name(),
                   uri          :: ersip_uri:uri(),
                   hparams      :: ersip_hparams:hparams()}).
-type refer_to() :: #refer_to{}.
-type parse_result() :: {ok, refer_to()} | {error, term()}.

%%===================================================================
%% API
%%===================================================================

-spec new(ersip_uri:uri()) -> refer_to().
new(URI) ->
    #refer_to{display_name = {display_name, []},
              uri = URI,
              hparams = ersip_hparams:new()}.

-spec uri(refer_to()) -> ersip_uri:uri().
uri(#refer_to{uri = URI}) ->
    URI.

-spec set_uri(ersip_uri:uri(), refer_to()) -> refer_to().
set_uri(URI, #refer_to{} = RT) ->
    RT#refer_to{uri = URI}.

-spec display_name(refer_to()) -> ersip_nameaddr:display_name().
display_name(#refer_to{display_name = DN}) ->
    DN.

-spec set_display_name(ersip_nameaddr:display_name(), refer_to()) -> refer_to().
set_display_name({display_name, _} = DN, #refer_to{} = RT) ->
    RT#refer_to{display_name = DN}.

-spec all_raw_params(refer_to()) -> [{binary(), binary()} | binary()].
all_raw_params(#refer_to{hparams = HParams}) ->
    ersip_hparams:to_raw_list(HParams).

-spec parse(ersip_hdr:header() | binary()) -> parse_result().
parse(Bin) when is_binary(Bin) ->
    parse_hdr(Bin);
parse(Header) ->
    case ersip_hdr:raw_values(Header) of
        [] ->
            {error, no_refer_to};
        [ReferToIOList]  ->
            parse_hdr(iolist_to_binary(ReferToIOList))
    end.

-spec make(binary()) -> refer_to().
make(Bin) ->
    case parse_hdr(Bin) of
        {ok, ReferTo} ->
            ReferTo;
        {error, Reason} ->
            error(Reason)
    end.

-spec assemble(refer_to()) -> iolist().
assemble(#refer_to{} = ReferTo) ->
    #refer_to{display_name = DisplayName,
              uri = URI,
              hparams = HParams
             } = ReferTo,
    HParamsIO0 = ersip_hparams:assemble(HParams),
    HParamsIO =
        case ersip_iolist:is_empty(HParamsIO0) of
            true -> [];
            false -> [$; | HParamsIO0]
        end,
    [ersip_nameaddr:assemble(DisplayName, URI), HParamsIO].

-spec assemble_bin(refer_to()) -> binary().
assemble_bin(#refer_to{} = ReferTo) ->
    iolist_to_binary(assemble(ReferTo)).

-spec build(HdrName :: binary(), refer_to()) -> ersip_hdr:header().
build(HdrName, #refer_to{} = ReferTo) ->
    Hdr = ersip_hdr:new(HdrName),
    ersip_hdr:add_value(assemble(ReferTo), Hdr).

%%===================================================================
%% Internal implementation
%%===================================================================

-spec parse_hdr(binary()) -> parse_result().
parse_hdr(Bin) ->
    Parsers = [fun ersip_nameaddr:parse/1,
               fun ersip_parser_aux:trim_lws/1,
               fun parse_refer_to_params/1
              ],
    case ersip_parser_aux:parse_all(Bin, Parsers) of
        {ok, [{DisplayName, URI}, _, HParams], <<>>} ->
            ReferTo = #refer_to{display_name = DisplayName,
                                uri          = URI,
                                hparams      = HParams},
            {ok, ReferTo};
        {ok, _, Rest} ->
            {error, {invalid_refer_to, {garbage_at_end, Rest}}};
        {error, Reason} ->
            {error, {invalid_refer_to, Reason}}
    end.


-spec parse_refer_to_params(binary()) -> ersip_parser_aux:parse_result(ersip_hparams:hparams()).
parse_refer_to_params(<<$;, Bin/binary>>) ->
    do_parse_refer_to_params(Bin);
parse_refer_to_params(Bin) ->
    {ok, ersip_hparams:new(), Bin}.

-spec do_parse_refer_to_params(binary()) -> ersip_parser_aux:parse_result(ersip_hparams:hparams()).
do_parse_refer_to_params(Bin) ->
    case ersip_parser_aux:parse_params($;, Bin) of
        {ok, PList, Rest} ->
            HParams =
                lists:foldl(fun({Key, Value}, HParams) ->
                                    ersip_hparams:set_raw(Key, Value, HParams)
                            end,
                            ersip_hparams:new(),
                            PList),
            {ok, HParams, Rest};
        {error, _} = Error ->
            Error
    end.

