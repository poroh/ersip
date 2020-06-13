%%%
%%% Copyright (c) 2019, 2020 Dmitry Poroh
%%% All rights reserved.
%%% Distributed under the terms of the MIT License. See the LICENSE file.
%%%
%%% SIP Subscription-State header
%%% RFC 6665
%%%

-module(ersip_hdr_subscription_state).

-export([value/1,
         event_reason_value/1,
         event_reason_value/2,
         set_event_reason_value/2,
         expires/1,
         expires/2,
         set_expires/2,
         retry_after/1,
         retry_after/2,
         set_retry_after/2,
         param/2,
         set_param/3,

         make/1,
         parse/1,
         assemble/1,
         assemble_bin/1,
         build/2,
         raw/1
        ]).

-export_type([subs_state/0,
              value/0,
              event_reason_value/0,
              raw/0
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
-type event_reason_value() :: deactivated
                            | probation
                            | rejected
                            | timeout
                            | giveup
                            | noresource
                            | invariant
                            | {unknown_reason, binary()}.
-type raw() :: #{value  := binary(),
                 params := ersip_hparams:raw(),
                 reason => binary(),
                 retry_after => non_neg_integer(),
                 expires => non_neg_integer()}.

%%===================================================================
%% API
%%===================================================================

%% @doc Main value of the subscription state (active/pending/terminated).
-spec value(subs_state()) -> value().
value(#subs_state{value = V}) ->
    V.

%% @doc Value of reason parameter (deactivated/probation/rejected/noresource/...)
-spec event_reason_value(subs_state()) -> event_reason_value() | undefined.
event_reason_value(#subs_state{hparams = HParams}) ->
    case ersip_hparams:find(reason, HParams) of
        not_found -> undefined;
        {ok, V} -> V
    end.

%% @doc Value of reason parameter (deactivated/probation/rejected/noresource/...)
-spec event_reason_value(subs_state(), event_reason_value() | undefined) -> event_reason_value() | undefined.
event_reason_value(#subs_state{hparams = HParams}, Default) ->
    case ersip_hparams:find(reason, HParams) of
        not_found ->
            Default;
        {ok, V} ->
            V
    end.

%% @doc Set value of reason parameter.
-spec set_event_reason_value(binary() | event_reason_value(), subs_state()) -> subs_state().
set_event_reason_value({unknown_reason, Val}, #subs_state{} = SubsState) ->
    set_param(<<"reason">>, Val, SubsState);
set_event_reason_value(Val, #subs_state{} = SubsState) when is_binary(Val) ->
    set_param(<<"reason">>, Val, SubsState);
set_event_reason_value(Atom, #subs_state{} = SubsState) when is_atom(Atom) ->
    set_param(<<"reason">>, atom_to_binary(Atom, utf8), SubsState).


%% @doc Value of the expires prameter.
-spec expires(subs_state()) -> non_neg_integer() | undefined.
expires(#subs_state{hparams = HParams}) ->
    case ersip_hparams:find(expires, HParams) of
        not_found -> undefined;
        {ok, V} -> V
    end.

%% @doc Value of the expires prameter.
-spec expires(subs_state(), non_neg_integer() | undefined) -> non_neg_integer() | undefined.
expires(#subs_state{hparams = HParams}, Default) ->
    case ersip_hparams:find(expires, HParams) of
        not_found ->
            Default;
        {ok, V} ->
            V
    end.

-spec set_expires(non_neg_integer(), subs_state()) -> subs_state().
set_expires(Expires, #subs_state{} = SubsState) when Expires >= 0 ->
    set_param(<<"expires">>, integer_to_binary(Expires), SubsState).

%% @doc Value of the retry-after parameter.
-spec retry_after(subs_state()) -> non_neg_integer() | undefined.
retry_after(#subs_state{hparams = HParams}) ->
    case ersip_hparams:find(retry_after, HParams) of
        not_found -> undefined;
        {ok, V} -> V
    end.

%% @doc Value of the retry-after parameter.
-spec retry_after(subs_state(), non_neg_integer() | undefined) -> non_neg_integer() | undefined.
retry_after(#subs_state{hparams = HParams}, Default) ->
    case ersip_hparams:find(retry_after, HParams) of
        not_found ->
            Default;
        {ok, V} ->
            V
    end.

%% @doc Set retry after parameter.
-spec set_retry_after(non_neg_integer(), subs_state()) -> subs_state().
set_retry_after(RetryAfter, #subs_state{} = SubsState) when RetryAfter >= 0 ->
    set_param(<<"retry-after">>, integer_to_binary(RetryAfter), SubsState).

%% @doc Value of any parameter in raw form.
-spec param(Name :: binary(), subs_state()) -> {ok, Value :: binary()} | not_found.
param(Name, #subs_state{hparams = HParams}) when is_binary(Name) ->
    ersip_hparams:find_raw(Name, HParams).

%% @doc Set value of any parameter in raw form.
-spec set_param(Name :: binary(), PValue :: binary(), subs_state()) -> subs_state().
set_param(PName, PValue, #subs_state{hparams = HParams} = SubsState)
        when is_binary(PName), is_binary(PValue) ->
    case ersip_hparams:set(PName, PValue, fun parse_known/2, HParams) of
        {ok, NewHParam} ->
            SubsState#subs_state{hparams = NewHParam};
        {error, Reason} ->
            error(Reason)
    end.

%% @doc Make Subscription-State header from binary or raw
%% representation of parameter.
-spec make(binary()) -> subs_state().
make(Bin) when is_binary(Bin) ->
    case parse(Bin) of
        {ok, SubsState} ->
            SubsState;
        {error, Reason} ->
            error(Reason)
    end;
make(#{value := ValueBin} = Raw) ->
    Value    = decode_value(ValueBin),
    HParams0 = ersip_hparams:make(maps:get(params, Raw, #{})),
    HParams =
        case ersip_hparams:parse_known(fun parse_known/2, HParams0) of
            {ok, H} -> H;
            {error, Reason} -> error({invalid_params, Reason})
        end,
    SubsState0 = #subs_state{value = Value,
                             valuebin = ValueBin,
                             hparams       = HParams
                            },
    Opts = [{expires,     fun(X, SState) -> set_expires(X, SState) end},
            {retry_after, fun(X, SState) -> set_retry_after(X, SState) end},
            {reason,      fun(X, SState) -> set_event_reason_value(X, SState) end}
           ],
    ersip_map:apply_to(Opts, Raw, SubsState0).


%% @doc Parse Subscription-State header from binary or raw SIP header.
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

%% @doc Assemble Subscription-State to iolist().
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

%% @doc Assemble Subscription-State to binary().
-spec assemble_bin(subs_state()) -> binary().
assemble_bin(#subs_state{} = SubsState) ->
    iolist_to_binary(assemble(SubsState)).

%% @doc Build raw SIP header from Subscription-State header.
-spec build(HdrName :: binary(), subs_state()) -> ersip_hdr:header().
build(HdrName, #subs_state{} = SubsState) ->
    Hdr = ersip_hdr:new(HdrName),
    ersip_hdr:add_value(assemble(SubsState), Hdr).

%% @doc Raw representation of Subscription-State header.
-spec raw(subs_state()) -> raw().
raw(#subs_state{hparams = HParams} = SubsState) ->
    Raw = #{value =>
                case value(SubsState) of
                    {unknown, Val} -> Val;
                    Val -> atom_to_binary(Val, utf8)
                end,
            params => ersip_hparams:raw(HParams)
           },
    Opts = [{expires,     expires(SubsState, undefined), fun(X) -> X end},
            {retry_after, retry_after(SubsState, undefined), fun(X) -> X end},
            {reason,      event_reason_value(SubsState, undefined),
             fun({unknown_reason, X}) -> X;
                (Reason) -> atom_to_binary(Reason, utf8)
             end}],
    OptsKVP = [{K, F(V)} || {K, V, F} <- Opts, V /= undefined],
    maps:merge(maps:from_list(OptsKVP), Raw).

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

%% @private
-spec parse_params(binary()) -> ersip_parser_aux:parse_result(ersip_parser_aux:gen_param_list()).
parse_params(<<$;, Bin/binary>>) ->
    ersip_hparams:parse(fun parse_known/2, Bin);
parse_params(Bin) ->
    {ok, ersip_hparams:new(), Bin}.

%% @private
-spec parse_known(binary(), binary()) -> ersip_hparams:parse_known_fun_result().
parse_known(<<"reason">>, Value) ->
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
            {ok, {reason, decode_event_reason(Value)}};
        {ok, _, _} ->
            {error, {invalid_event_reason, Value}};
        {error, Reason} ->
            {error, {invalid_event_reason, Reason}}
    end;
parse_known(<<"expires">>, Value) ->
    %% ("expires" EQUAL delta-seconds)
    case parse_delta_seconds(Value) of
        {ok, Expires} -> {ok, {expires, Expires}};
        {error, Reason} ->
            {error, {invalid_event_expires, Reason}}
    end;
parse_known(<<"retry-after">>, Value) ->
    %% ("retry-after" EQUAL delta-seconds)
    case parse_delta_seconds(Value) of
        {ok, RetryAfter} -> {ok, {retry_after, RetryAfter}};
        {error, Reason} ->
            {error, {invalid_retry_after, Reason}}
    end;
parse_known(_, _) ->
    {ok, unknown}.

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
