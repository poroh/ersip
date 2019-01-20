%%%
%%% Copyright (c) 2019 Dmitry Poroh
%%% All rights reserved.
%%% Distributed under the terms of the MIT License. See the LICENSE file.
%%%
%%% SIP Subscription-State header
%%% RFC 6665
%%%

-module(ersip_hdr_subscription_state).

-export([value/1,
         event_reason_value/2,
         expires/2,
         retry_after/2,
         param/2,
         set_param/3,

         make/1,
         parse/1,
         assemble/1,
         assemble_bin/1,
         build/2
        ]).

-export_type([subs_state/0,
              value/0
             ]).

%%===================================================================
%% Types
%%===================================================================

-record(subs_state, {value    :: value(),
                     valuebin :: binary(),
                     hparams  :: ersip_hparams:hparams()
                    }).
-type subs_state()  :: #subs_state{}.
-type value()       :: active | pending | terminated | {unknown, binary()}.
-type known_param() :: reason | expires | retry_after.
-type event_reason_value() :: deactivated
                            | probation
                            | rejected
                            | timeout
                            | giveup
                            | noresource
                            | invariant
                            | {unknown_reason, binary()}.

%%===================================================================
%% API
%%===================================================================

-spec value(subs_state()) -> value().
value(#subs_state{value = V}) ->
    V.

-spec event_reason_value(subs_state(), event_reason_value() | undefined) -> event_reason_value() | undefined.
event_reason_value(#subs_state{hparams = HParams}, Default) ->
    case ersip_hparams:find(reason, HParams) of
        not_found ->
            Default;
        {ok, V} ->
            V
    end.

-spec expires(subs_state(), non_neg_integer() | undefined) -> non_neg_integer() | undefined.
expires(#subs_state{hparams = HParams}, Default) ->
    case ersip_hparams:find(expires, HParams) of
        not_found ->
            Default;
        {ok, V} ->
            V
    end.

-spec retry_after(subs_state(), non_neg_integer() | undefined) -> non_neg_integer() | undefined.
retry_after(#subs_state{hparams = HParams}, Default) ->
    case ersip_hparams:find(retry_after, HParams) of
        not_found ->
            Default;
        {ok, V} ->
            V
    end.

-spec param(Name :: binary(), subs_state()) -> {ok, Value :: binary()} | not_found.
param(Name, #subs_state{hparams = HParams}) when is_binary(Name) ->
    ersip_hparams:find_raw(Name, HParams).

-spec set_param(Name :: binary(), PValue :: binary(), subs_state()) -> subs_state().
set_param(PName, PValue, #subs_state{hparams = HParams} = SubsState)
        when is_binary(PName), is_binary(PValue) ->
    case set_hparam(PName, PValue, HParams) of
        {ok, NewHParam} ->
            SubsState#subs_state{hparams = NewHParam};
        {error, Reason} ->
            error(Reason)
    end.

-spec make(binary()) -> subs_state().
make(Bin) when is_binary(Bin) ->
    case parse(Bin) of
        {ok, SubsState} ->
            SubsState;
        {error, Reason} ->
            error(Reason)
    end.

-spec parse(binary() | ersip_hdr:header()) -> {ok, subs_state()} | {error, term()}.
parse(Bin) when is_binary(Bin) ->
    Parsers = [fun ersip_parser_aux:parse_token/1,
               fun ersip_parser_aux:trim_lws/1,
               fun parse_params/1
              ],
    case ersip_parser_aux:parse_all(Bin, Parsers) of
        {ok, [ValueBin, _, HParams], <<>>} ->
            SubsState = #subs_state{value    = decode_value(ValueBin),
                                    valuebin = ValueBin,
                                    hparams  = HParams},
            {ok, SubsState};
        {ok, _, Rest} ->
            {error, {invalid_subscription_state, {garbage_at_the_end, Rest}}};
        {error, Reason} ->
            {error, {invalid_subscription_state, Reason}}
    end;
parse(Header) ->
    case ersip_hdr:raw_values(Header) of
        [] ->
            {error, no_subsciption_state};
        [SubsStateIOList]  ->
            parse(iolist_to_binary(SubsStateIOList))
    end.

-spec assemble(subs_state()) -> iolist().
assemble(#subs_state{} = SubsState) ->
    #subs_state{valuebin = ValueBin,
                hparams  = HParams} = SubsState,
    HParamsIO0 = ersip_hparams:assemble(HParams),
    HParamsIO =
        case ersip_iolist:is_empty(HParamsIO0) of
            true -> [];
            false -> [$; | HParamsIO0]
        end,
    [ValueBin, HParamsIO].

-spec assemble_bin(subs_state()) -> binary().
assemble_bin(#subs_state{} = SubsState) ->
    iolist_to_binary(assemble(SubsState)).

-spec build(HdrName :: binary(), subs_state()) -> ersip_hdr:header().
build(HdrName, #subs_state{} = SubsState) ->
    Hdr = ersip_hdr:new(HdrName),
    ersip_hdr:add_value(assemble(SubsState), Hdr).

%%===================================================================
%% Internal Implementation
%%===================================================================

-spec decode_value(binary()) -> value().
decode_value(Bin) ->
    case ersip_bin:to_lower(Bin) of
        <<"active">>     -> active;
        <<"pending">>    -> pending;
        <<"terminated">> -> terminated;
        Lower ->
            {unknown, Lower}
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

%% subexp-params        =   ("reason" EQUAL event-reason-value)
%%                        / ("expires" EQUAL delta-seconds)
%%                        / ("retry-after" EQUAL delta-seconds)
%%                        / generic-param
-spec param_name_to_atom(binary()) -> {ok, known_param()}
                                     | not_found
                                     | {error, {invalid_param, binary()}}.
param_name_to_atom(<<"reason">>)      -> {ok, reason};
param_name_to_atom(<<"expires">>)     -> {ok, expires};
param_name_to_atom(<<"retry-after">>) -> {ok, retry_after};
param_name_to_atom(X) when is_binary(X) ->
    case ersip_parser_aux:check_token(X) of
        true -> not_found;
        false -> {error, {invalid_param, X}}
    end.

-spec parse_param(known_param(), binary()) -> {ok, Value} | {error, Err} when
      Value :: integer()| ersip_qvalue:qvalue(),
      Err   :: {invalid_event_reason, binary()}
             | {invalid_qvalue, binary()}.
parse_param(reason, Value) ->
    %% "reason" EQUAL event-reason-value
    %% event-reason-value   = "deactivated"
    %%                      / "probation"
    %%                      / "rejected"
    %%                      / "timeout"
    %%                      / "giveup"
    %%                      / "noresource"
    %%                      / "invariant"
    %%                      / event-reason-extension
    %% event-reason-extension = token
    case ersip_parser_aux:parse_token(Value) of
        {ok, Value, <<>>} ->
            {ok, decode_event_reason(Value)};
        {ok, _, _} ->
            {error, {invalid_event_reason, Value}};
        {error, Reason} ->
            {error, {invalid_event_reason, Reason}}
    end;
parse_param(expires, Value) ->
    %% ("expires" EQUAL delta-seconds)
    case parse_delta_seconds(Value) of
        {ok, _} = Ok -> Ok;
        {error, Reason} ->
            {error, {invalid_event_expires, Reason}}
    end;
parse_param(retry_after, Value) ->
    %% ("retry-after" EQUAL delta-seconds)
    case parse_delta_seconds(Value) of
        {ok, _} = Ok -> Ok;
        {error, Reason} ->
            {error, {invalid_retry_after, Reason}}
    end.

-spec decode_event_reason(binary()) -> event_reason_value().
decode_event_reason(Bin) ->
    case ersip_bin:to_lower(Bin) of
        <<"deactivated">> -> deactivated;
        <<"probation">>   -> probation;
        <<"rejected">>    -> rejected;
        <<"timeout">>     -> timeout;
        <<"giveup">>      -> giveup;
        <<"noresource">>  -> noresource;
        <<"invariant">>   -> invariant;
        Lower ->
            {unknown_reason, Lower}
    end.

%% RFC 2068:
%% delta-seconds  = 1*DIGIT
-spec parse_delta_seconds(binary()) -> {ok, non_neg_integer()} | {error, term()}.
parse_delta_seconds(Bin) ->
    case ersip_parser_aux:parse_non_neg_int(Bin) of
        {ok, Value, <<>>} ->
            {ok, Value};
        {ok, _, _} ->
            {error, {invalid_integer, Bin}};
        {error, _} = Error ->
            Error
    end.
