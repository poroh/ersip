%%%
%%% Copyright (c) 2019 Dmitry Poroh
%%% All rights reserved.
%%% Distributed under the terms of the MIT License. See the LICENSE file.
%%%
%%% SIP Event header
%%% RFC 6665
%%%

-module(ersip_hdr_event).

-export([type/1,
         type_bin/1,
         id/2,
         param/2,
         set_param/3,

         make/1,
         parse/1,
         assemble/1,
         assemble_bin/1,
         build/2
        ]).

-export_type([type/0, id/0]).

%%===================================================================
%% Types
%%===================================================================

-record(event, {type    :: {unknown_event, binary()},
                typebin :: binary(),
                hparams :: ersip_hparams:hparams()
               }).
-type event() :: #event{}.
-type type()  :: {unknown_event, binary()}.
-type known_param() :: id.
-type id()   :: binary(). %% lower case.

%%===================================================================
%% API
%%===================================================================

-spec type(event()) -> type().
type(#event{type = T}) ->
    T.

-spec type_bin(event()) -> binary().
type_bin(#event{type = {unknown_event, V}}) ->
    V.

%% Get event identifier (lower case).
-spec id(event(), id() | undefined) -> id() | undefined.
id(#event{hparams = HParams}, Default) ->
    case ersip_hparams:find(id, HParams) of
        not_found ->
            Default;
        {ok, V} ->
            V
    end.

-spec param(Name :: binary(), event()) -> {ok, Value :: binary()} | not_found.
param(Name, #event{hparams = HParams}) when is_binary(Name) ->
    ersip_hparams:find_raw(Name, HParams).

-spec set_param(Name :: binary(), PValue :: binary(), event()) -> event().
set_param(PName, PValue, #event{hparams = HParams} = Event)
        when is_binary(PName), is_binary(PValue) ->
    case set_hparam(PName, PValue, HParams) of
        {ok, NewHParam} ->
            Event#event{hparams = NewHParam};
        {error, Reason} ->
            error(Reason)
    end.

-spec make(binary()) -> event().
make(Bin) when is_binary(Bin) ->
    case parse(Bin) of
        {ok, Event} ->
            Event;
        {error, Reason} ->
            error(Reason)
    end.

-spec parse(binary() | ersip_hdr:header()) -> ersip_parser_aux:parse_result(event()).
parse(Bin) when is_binary(Bin) ->
    Parsers = [fun ersip_parser_aux:parse_token/1,
               fun ersip_parser_aux:trim_lws/1,
               fun parse_params/1
              ],
    case ersip_parser_aux:parse_all(Bin, Parsers) of
        {ok, [TypeBin, _, HParams], <<>>} ->
            Event = #event{type     = decode_type(TypeBin),
                           typebin  = TypeBin,
                           hparams  = HParams},
            {ok, Event};
        {ok, _, Rest} ->
            {error, {invalid_event, {garbage_at_the_end, Rest}}};
        {error, Reason} ->
            {error, {invalid_event, Reason}}
    end;
parse(Header) ->
    case ersip_hdr:raw_values(Header) of
        [] ->
            {error, no_event};
        [EventIOList]  ->
            parse(iolist_to_binary(EventIOList))
    end.

-spec assemble(event()) -> iolist().
assemble(#event{} = Event) ->
    #event{typebin  = TypeBin,
           hparams  = HParams} = Event,
    HParamsIO0 = ersip_hparams:assemble(HParams),
    HParamsIO =
        case ersip_iolist:is_empty(HParamsIO0) of
            true -> [];
            false -> [$; | HParamsIO0]
        end,
    [TypeBin, HParamsIO].

-spec assemble_bin(event()) -> binary().
assemble_bin(#event{} = Event) ->
    iolist_to_binary(assemble(Event)).

-spec build(HdrName :: binary(), event()) -> ersip_hdr:header().
build(HdrName, #event{} = Event) ->
    Hdr = ersip_hdr:new(HdrName),
    ersip_hdr:add_value(assemble(Event), Hdr).

%%===================================================================
%% Internal Implementation
%%===================================================================

-spec decode_type(binary()) -> type().
decode_type(Bin) ->
    case ersip_bin:to_lower(Bin) of
        Lower ->
            {unknown_event, Lower}
    end.

-spec parse_params(binary()) -> ersip_parser_aux:parse_result(ersip_hparams:hparams()).
parse_params(<<$;, Bin/binary>>) ->
    do_parse_params(Bin);
parse_params(Bin) ->
    {ok, ersip_hparams:new(), Bin}.

-spec do_parse_params(binary()) -> ersip_parser_aux:parse_result(ersip_hparams:hparams()).
do_parse_params(Bin) ->
    case ersip_parser_aux:parse_params($;, Bin) of
        {ok, PList, Rest} ->
            try
                HParams =
                    lists:foldl(fun({Key, Value}, HParams) ->
                                        case set_hparam(Key, Value, HParams) of
                                            {ok, NewHParam} ->
                                                NewHParam;
                                            {error, _} = Error ->
                                                throw(Error)
                                        end
                                end,
                                ersip_hparams:new(),
                                PList),
                {ok, HParams, Rest}
            catch
                throw:{error, _} = Error ->
                    Error
            end;
        {error, _} = Error ->
            Error
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
        {error, _} = Error ->
            Error;
        not_found ->
            {ok, ersip_hparams:set_raw(PName, PValue, HParams)}
    end.

%% event-param       =  generic-param / ( "id" EQUAL token )
-spec param_name_to_atom(binary()) -> {ok, known_param()}
                                     | not_found
                                     | {error, {invalid_param, binary()}}.
param_name_to_atom(<<"id">>) -> {ok, id};
param_name_to_atom(X) when is_binary(X) ->
    case ersip_parser_aux:check_token(X) of
        true -> not_found;
        false -> {error, {invalid_param, X}}
    end.

-spec parse_param(known_param(), binary()) -> {ok, Value} | {error, Err} when
      Value :: integer()| ersip_qvalue:qvalue(),
      Err   :: {invalid_event_reason, binary()}
             | {invalid_qvalue, binary()}.
parse_param(id, Value) ->
    %% "id" EQUAL token
    case ersip_parser_aux:parse_token(Value) of
        {ok, Value, <<>>} ->
            {ok, ersip_bin:to_lower(Value)};
        {ok, _, _} ->
            {error, {invalid_event_id, Value}};
        {error, Reason} ->
            {error, {invalid_event_id, Reason}}
    end.
