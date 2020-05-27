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
         build/2,

         raw/1
        ]).

-export_type([type/0, id/0, raw/0]).

%%===================================================================
%% Types
%%===================================================================

-record(event, {type    :: {unknown_event, binary()},
                typebin :: binary(),
                hparams :: ersip_hparams:hparams()
               }).
-type event() :: #event{}.
-type type()  :: {unknown_event, binary()}.
-type id()    :: binary(). %% lower case.
-type raw()   :: #{type   := binary(), %% lower case
                   params := ersip_hparams:raw(),
                   id     => binary() %% lower case,
                  }.

%%===================================================================
%% API
%%===================================================================

%% @doc Get event type.
-spec type(event()) -> type().
type(#event{type = T}) ->
    T.

%% @doc Get event type in binary form.
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

%% Set event identifier.
%% Raises error if first parameter is not valid identifier.
-spec set_id(binary(), event()) -> event().
set_id(Value, #event{hparams = HParams} = Event) ->
    case ersip_hparams:set(<<"id">>, Value, fun parse_known/2, HParams) of
        {ok, NewHParam} ->
            Event#event{hparams = NewHParam};
        {error, Reason} ->
            error(Reason)
    end.

%% @doc Get raw parameter value.
-spec param(Name :: binary(), event()) -> {ok, Value :: binary()} | not_found.
param(Name, #event{hparams = HParams}) when is_binary(Name) ->
    ersip_hparams:find_raw(Name, HParams).

%% @doc Set raw parameter value.
-spec set_param(Name :: binary(), PValue :: binary(), event()) -> event().
set_param(PName, PValue, #event{hparams = HParams} = Event)
        when is_binary(PName), is_binary(PValue) ->
    case ersip_hparams:set(PName, PValue, fun parse_known/2, HParams) of
        {ok, NewHParam} ->
            Event#event{hparams = NewHParam};
        {error, Reason} ->
            error(Reason)
    end.

%% @doc Create Event header from SIP or from raw representation.
-spec make(binary()) -> event().
make(Bin) when is_binary(Bin) ->
    case parse(Bin) of
        {ok, Event} ->
            Event;
        {error, Reason} ->
            error(Reason)
    end;
make(#{type := TypeBin} = Raw) when is_binary(TypeBin) ->
    case ersip_parser_aux:check_token(TypeBin) of
        true -> ok;
        false -> error({invalid_type, TypeBin})
    end,
    Type = decode_type(TypeBin),
    HParams0 = ersip_hparams:make(maps:get(params, Raw, #{})),
    HParams =
        case ersip_hparams:parse_known(fun parse_known/2, HParams0) of
            {ok, H} -> H;
            {error, Reason} -> error({invalid_params, Reason})
        end,
    Ev0 = #event{type = Type, typebin = TypeBin, hparams = HParams},
    case Raw of
        #{id := Id} -> set_id(Id, Ev0);
        _ -> Ev0
    end.

%% @doc Parse event header from binary form or from SIP raw header.
-spec parse(binary() | ersip_hdr:header()) -> {ok, event()} | {error, term()}.
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

%% @doc Serialize header to iolist.
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

%% @doc Serialize header to binary.
-spec assemble_bin(event()) -> binary().
assemble_bin(#event{} = Event) ->
    iolist_to_binary(assemble(Event)).

%% @doc Build raw SIP header.
-spec build(HdrName :: binary(), event()) -> ersip_hdr:header().
build(HdrName, #event{} = Event) ->
    Hdr = ersip_hdr:new(HdrName),
    ersip_hdr:add_value(assemble(Event), Hdr).

%% @doc Get raw Erlang term representation.
-spec raw(event()) -> raw().
raw(#event{hparams = HParams} = Ev) ->
    Base = #{type   => type_bin(Ev),
             params => ersip_hparams:raw(HParams)},
    case id(Ev, undefined) of
        undefined -> Base;
        Id -> Base#{id => Id}
    end.

%%===================================================================
%% Internal Implementation
%%===================================================================

%% @private
-spec decode_type(binary()) -> type().
decode_type(Bin) ->
    case ersip_bin:to_lower(Bin) of
        Lower ->
            {unknown_event, Lower}
    end.

%% @private
-spec parse_known(binary(), binary()) -> ersip_hparams:parse_known_fun_result().
parse_known(<<"id">>, Value) ->
    %% "id" EQUAL token
    case ersip_parser_aux:parse_token(Value) of
        {ok, Value, <<>>} -> {ok, {id, ersip_bin:to_lower(Value)}};
        {ok, _, _}        -> {error, {invalid_event_id, Value}};
        {error, Reason}   -> {error, {invalid_event_id, Reason}}
    end;
parse_known(_, _) ->
    {ok, unknown}.

%% @private
-spec parse_params(binary()) -> ersip_parser_aux:parse_result(ersip_hparams:hparams()).
parse_params(<<$;, Bin/binary>>) ->
    do_parse_params(Bin);
parse_params(Bin) ->
    {ok, ersip_hparams:new(), Bin}.

%% @private
-spec do_parse_params(binary()) -> ersip_parser_aux:parse_result(ersip_hparams:hparams()).
do_parse_params(Bin) ->
    case ersip_hparams:parse_raw(Bin) of
        {ok, HParams0, Rest} ->
            case ersip_hparams:parse_known(fun parse_known/2, HParams0) of
                {ok, HParams} ->
                    {ok, HParams, Rest};
                {error, _} = Error ->
                    Error
            end;
        {error, _} = Error ->
            Error
    end.
