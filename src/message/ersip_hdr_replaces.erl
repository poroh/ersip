%%
%% Copyright (c) 2019 Dmitry Poroh
%% All rights reserved.
%% Distributed under the terms of the MIT License. See the LICENSE file.
%%
%% Replaces header support (RFC 3891)
%%

-module(ersip_hdr_replaces).

-export([dialog_id/1,
         early_only/1,
         make/1,
         parse/1,
         build/2,
         assemble/1,
         assemble_bin/1]).

-export_type([replaces/0]).

%%%===================================================================
%%% Types
%%%===================================================================

-record(replaces, {call_id :: ersip_hdr_callid:callid(),
                   params  :: ersip_hparams:hparams()}).
-type replaces() :: #replaces{}.
-type known_param() :: from_tag | to_tag | early_only.

%%%===================================================================
%%% API
%%%===================================================================

-spec dialog_id(replaces()) -> ersip_dialog:id().
dialog_id(#replaces{call_id = CallId, params = Params}) ->
    %% In other words, the to-tag parameter is compared to the local
    %% tag, and the from-tag parameter is compared to the remote tag.
    FromTagKey = ersip_hdr_fromto:tag_key(ersip_hparams:get(from_tag, Params)),
    ToTagKey = ersip_hdr_fromto:tag_key(ersip_hparams:get(to_tag, Params)),
    ersip_dialog:make_id(ToTagKey, FromTagKey, CallId).


-spec early_only(replaces()) -> boolean().
early_only(#replaces{params = P}) ->
    case ersip_hparams:find(early_only, P) of
        not_found -> false;
        {ok, true} -> true
    end.

-spec make(binary()) -> replaces().
make(HeaderBin) when is_binary(HeaderBin) ->
    case parse(HeaderBin) of
        {ok, Replaces} ->
            Replaces;
        {error, Reason} ->
            error(Reason)
    end.

-spec parse(ersip_hdr:header() | binary()) -> {ok, replaces()} | {error, term()}.
parse(HeaderBin) when is_binary(HeaderBin) ->
    parse_replaces(HeaderBin);
parse(Header) ->
    case ersip_hdr:raw_values(Header) of
        [] ->
            {error, no_replaces};
        [ReplacesIOList]  ->
            parse_replaces(iolist_to_binary(ReplacesIOList))
    end.

-spec build(HdrName, replaces()) -> ersip_hdr:header() when
      HdrName :: binary().
build(HdrName, #replaces{} = R) ->
    Hdr = ersip_hdr:new(HdrName),
    IOValue = assemble(R),
    ersip_hdr:add_value(IOValue, Hdr).

-spec assemble(replaces()) -> iolist().
assemble(#replaces{call_id = CallId, params = Params}) ->
    [ersip_hdr_callid:assemble(CallId),
     $;, ersip_hparams:assemble(Params)].

-spec assemble_bin(replaces()) -> binary().
assemble_bin(#replaces{} = R) ->
    iolist_to_binary(assemble(R)).

%%%===================================================================
%%% Internal implementation
%%%===================================================================

-spec param_name_to_atom(binary()) -> {ok, known_param()} | not_found.
param_name_to_atom(<<"to-tag">>)     -> {ok, to_tag};
param_name_to_atom(<<"from-tag">>)   -> {ok, from_tag};
param_name_to_atom(<<"early-only">>) -> {ok, early_only};
param_name_to_atom(X) when is_binary(X) -> not_found.


-spec parse_replaces(binary()) -> {ok, replaces()} | {error, term()}.
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
    case ersip_parser_aux:parse_params($;, Bin) of
        {error, _} = Error -> Error;
        {ok, PList, <<>>} ->
            try
                HParams =
                    lists:foldl(
                      fun({Key, Value}, HParams) ->
                              case set_hparam(Key, Value, HParams) of
                                  {ok, NewHParam} ->
                                      NewHParam;
                                  {error, _} = Error ->
                                      throw(Error)
                              end
                      end,
                      ersip_hparams:new(),
                      PList),
                {ok, HParams}
            catch
                throw:{error, _} = Error ->
                    Error
            end;
        {ok, _, Rest} ->
            {error, {invalid_replaces, {garbage_at_end, Rest}}}
    end.


-spec set_hparam(Name :: binary(), PValue :: binary(), ersip_hparams:hparams()) -> Result when
      Result :: {ok, ersip_hparams:hparams()}
              | {error, term()}.
set_hparam(PName, PValue, HParams) ->
    LowerName = ersip_bin:to_lower(PName),
    case param_name_to_atom(LowerName) of
        {ok, ParsedName} ->
            case parse_param(ParsedName, PValue) of
                {ok, ParsedValue} ->
                    {ok, ersip_hparams:set(ParsedName, ParsedValue, PName, PValue, HParams)};
                {error, _} = Error ->
                    Error
            end;
        not_found ->
            {ok, ersip_hparams:set_raw(PName, PValue, HParams)}
    end.

-spec parse_param(known_param(), binary()) -> {ok, Value} | {error, term()} when
      Value :: ersip_hdr_fromto:tag() | true.
parse_param(from_tag, Value) ->
    case ersip_parser_aux:check_token(Value) of
        true -> {ok, {tag, Value}};
        false -> {error, {invalid_from_tag, Value}}
    end;
parse_param(to_tag, Value) ->
    case ersip_parser_aux:check_token(Value) of
        true -> {ok, {tag, Value}};
        false -> {error, {invalid_to_tag, Value}}
    end;
parse_param(early_only, <<>>) ->
    {ok, true};
parse_param(early_only, Value) ->
    {error, {invalid_early_only, Value}}.


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

