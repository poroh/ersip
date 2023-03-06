%%%
%%% Copyright (c) 2019, 2020 Dmitry Poroh
%%% All rights reserved.
%%% Distributed under the terms of the MIT License. See the LICENSE file.
%%%
%%% Replaces header support (RFC 3891)
%%%

-module(ersip_hdr_replaces).

-export([dialog_id/1,
         early_only/1,
         make/1,
         parse/1,
         build/2,
         assemble/1,
         assemble_bin/1,
         raw/1
        ]).

-export_type([replaces/0, raw/0]).

%%===================================================================
%% Types
%%===================================================================

-record(replaces, {call_id :: ersip_hdr_callid:callid(),
                   params  :: ersip_hparams:hparams()}).
-type replaces() :: #replaces{}.
-type parse_result() :: {ok, replaces()} | {error, parse_error()}.
-type parse_error() :: term().
-type raw() :: #{call_id   := binary(),
                 from_tag := binary(),
                 to_tag   := binary(),
                 early_only := boolean(),
                 params   := ersip_hparams:raw()
                }.

%%===================================================================
%% Api
%%===================================================================

%% @doc Dialog ID encoded in Replaces header.
-spec dialog_id(replaces()) -> ersip_dialog:id().
dialog_id(#replaces{call_id = CallId, params = Params}) ->
    %% In other words, the to-tag parameter is compared to the local
    %% tag, and the from-tag parameter is compared to the remote tag.
    FromTagKey = ersip_hdr_fromto:tag_key(ersip_hparams:get(from_tag, Params)),
    ToTagKey = ersip_hdr_fromto:tag_key(ersip_hparams:get(to_tag, Params)),
    ersip_dialog:make_id(ToTagKey, FromTagKey, CallId).

%% @doc Early only property of Replaces header.
-spec early_only(replaces()) -> boolean().
early_only(#replaces{params = P}) ->
    case ersip_hparams:find(early_only, P) of
        not_found -> false;
        {ok, true} -> true
    end.

%% @doc Create Replaces header from binary() or from raw
%% representation.
-spec make(binary()) -> replaces().
make(HeaderBin) when is_binary(HeaderBin) ->
    case parse(HeaderBin) of
        {ok, Replaces} ->
            Replaces;
        {error, Reason} ->
            error(Reason)
    end;
make(#{call_id := CallIdR, from_tag := FromTag, to_tag := ToTag} = Raw) ->
    CallId = ersip_hdr_callid:make(CallIdR),
    HParams0 = ersip_hparams:make(maps:get(params, Raw, #{})),
    HParams1 =
        case ersip_hparams:parse_known(fun parse_known/2, HParams0) of
            {ok, H} -> H;
            {error, Reason} -> error({invalid_param, Reason})
        end,
    HParams2 = ersip_hparams:set(from_tag, {tag, FromTag}, <<"from-tag">>, FromTag, HParams1),
    HParams3 = ersip_hparams:set(to_tag, {tag, ToTag}, <<"to-tag">>, ToTag, HParams2),
    HParams4 = case maps:get(early_only, Raw, false) of
                   true -> ersip_hparams:set(early_only, true, <<"early-only">>, <<>>, HParams3);
                   false -> HParams3
               end,
    #replaces{call_id  = CallId,
              params   = HParams4}.

%% @doc Parse Replaces header.
-spec parse(ersip_hdr:header() | binary()) -> parse_result().
parse(HeaderBin) when is_binary(HeaderBin) ->
    parse_replaces(HeaderBin);
parse(Header) ->
    case ersip_hdr:raw_values(Header) of
        [] ->
            {error, no_replaces};
        [ReplacesIOList]  ->
            parse_replaces(iolist_to_binary(ReplacesIOList))
    end.

%% @doc Build raw SIP header.
-spec build(binary(), replaces()) -> ersip_hdr:header().
build(HdrName, #replaces{} = R) ->
    Hdr = ersip_hdr:new(HdrName),
    IOValue = assemble(R),
    ersip_hdr:add_value(IOValue, Hdr).

%% @doc Assemble Replaces as iolist().
-spec assemble(replaces()) -> iolist().
assemble(#replaces{call_id = CallId, params = Params}) ->
    [ersip_hdr_callid:assemble(CallId),
     $;, ersip_hparams:assemble(Params)].

%% @doc Assemble Replaces as binary().
-spec assemble_bin(replaces()) -> binary().
assemble_bin(#replaces{} = R) ->
    iolist_to_binary(assemble(R)).

%% @doc Raw representation of Replaces header.
-spec raw(replaces()) -> raw().
raw(#replaces{call_id = CallId, params = HParams} = Replaces) ->
    {tag, FromTag} = ersip_hparams:get(from_tag, HParams),
    {tag, ToTag}   = ersip_hparams:get(to_tag, HParams),
    #{call_id => ersip_hdr_callid:raw(CallId),
      params => ersip_hparams:raw(HParams),
      early_only => early_only(Replaces),
      from_tag => FromTag,
      to_tag => ToTag
     }.

%%===================================================================
%% Internal implementation
%%===================================================================

-spec parse_replaces(binary()) -> parse_result().
parse_replaces(Binary) ->
    case binary:match(Binary, <<";">>) of
        nomatch ->
            {error, {invalid_replaces, no_tags}};
        {Pos, 1} ->
            <<CallIdBin:Pos/binary, $;, ParamsBin/binary>> = Binary,
            MaybeCallId = ersip_hdr_callid:parse(CallIdBin),
            MaybeParams = parse_params(ParamsBin),
            case {MaybeCallId, MaybeParams} of
                {{error, R}, _} -> {error, {invalid_replaces, R}};
                {_, {error, R}} -> {error, {invalid_replaces, R}};
                {{ok, CallId}, {ok, HParams}} ->
                    Replaces =
                        #replaces{call_id = CallId,
                                  params = HParams},
                    validate_replaces(Replaces)
            end
    end.

-spec parse_params(binary()) -> {ok, ersip_hparams:hparams()} | {error, term()}.
parse_params(Bin) ->
    case ersip_hparams:parse(fun parse_known/2, Bin) of
        {ok, HParams, <<>>} -> {ok, HParams};
        {ok, _, Rest} -> {error, {invalid_replaces, {garbage_at_end, Rest}}};
        {error, _} = Error -> Error
    end.

-spec validate_replaces(replaces()) -> {ok, replaces()} | {error, term()}.
validate_replaces(#replaces{params = Params} = R) ->
    %% A Replaces header field MUST contain exactly one to-tag and exactly
    %% one from-tag, as they are required for unique dialog matching.
    case ersip_hparams:find(from_tag, Params) of
        not_found -> {error, {invalid_replaces, no_from_tag}};
        {ok, _} ->
            case ersip_hparams:find(to_tag, Params) of
                not_found -> {error, {invalid_replaces, no_to_tag}};
                {ok, _} -> {ok, R}
            end
    end.

%% @private
-spec parse_known(binary(), binary()) -> ersip_hparams:parse_known_fun_result().
parse_known(<<"from-tag">>, Value) ->
    case ersip_parser_aux:check_token(Value) of
        true -> {ok, {from_tag, {tag, Value}}};
        false -> {error, {invalid_from_tag, Value}}
    end;
parse_known(<<"to-tag">>, Value) ->
    case ersip_parser_aux:check_token(Value) of
        true -> {ok, {to_tag, {tag, Value}}};
        false -> {error, {invalid_to_tag, Value}}
    end;
parse_known(<<"early-only">>, <<>>) ->
    {ok, {early_only, true}};
parse_known(<<"early-only">>, Value) ->
    {error, {invalid_early_only, Value}};
parse_known(_, _) ->
    {ok, unknown}.
