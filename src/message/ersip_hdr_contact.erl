%%%
%%% Copyright (c) 2018, 2019, 2020 Dmitry Poroh
%%% All rights reserved.
%%% Distributed under the terms of the MIT License. See the LICENSE file.
%%%
%%% SIP One SIP Contact entry
%%%

-module(ersip_hdr_contact).

-export([new/1,
         uri/1,
         display_name/1,
         expires/1,
         expires/2,
         set_expires/2,
         qvalue/1,
         qvalue/2,
         set_qvalue/2,
         param/2,
         set_param/3,
         all_raw_params/1,
         make/1,
         parse/1,
         parse_hdr/1,
         assemble/1,
         assemble_bin/1,
         raw/1
        ]).
-export_type([contact/0,
              raw/0,
              contact_param/0]).

%%===================================================================
%% Types
%%===================================================================

-record(contact, {display_name  = ersip_display_name:empty() :: ersip_display_name:display_name(),
                  uri           :: ersip_uri:uri(),
                  hparams       :: ersip_hparams:hparams()
                 }).
-type contact() :: #contact{}.

-type contact_param() :: {qvalue, ersip_qvalue:qvalue()}
                       | {expires, expires()}
                       | {binary(), binary()}.
-type expires() :: non_neg_integer().

-type parse_result() :: {ok, contact()} | {error, parse_error()}.
-type parse_error() ::  {invalid_contact, term()}.

-type raw() :: #{uri          := ersip_uri:raw(),
                 params       := ersip_hparams:raw(),
                 display_name := ersip_display_name:raw(),
                 expires      => expires(),
                 q            => ersip_qvalue:raw()}.

%%===================================================================
%% API
%%===================================================================

%% @doc Create Contact header from SIP URI.
-spec new(ersip_uri:uri()) -> contact().
new(URI) ->
    case ersip_uri:is_sip(URI) of
        true  -> #contact{uri = URI, hparams = ersip_hparams:new()};
        false -> error({sip_uri_expected, URI})
    end.

%% @doc Get URI from Contact header.
-spec uri(contact()) -> ersip_uri:uri().
uri(#contact{uri = URI}) ->
    URI.

%% @doc Get display name from Contact header.
-spec display_name(contact()) -> ersip_nameaddr:display_name().
display_name(#contact{display_name = DN}) ->
    DN.

%% @doc Get expires parameter value. If no expires parameter is
%% defined than undefined is retutned
-spec expires(contact(), Default :: expires()) -> expires() | undefined.
expires(#contact{hparams = HParams}, Default) ->
    case ersip_hparams:find(expires, HParams) of
        not_found -> Default;
        {ok, V} -> V
    end.

%% @doc Get expires parameter value.
%% If no expires parameter is in the header then Default is returned.
-spec expires(contact()) -> expires() | undefined.
expires(#contact{hparams = HParams}) ->
    case ersip_hparams:find(expires, HParams) of
        not_found -> undefined;
        {ok, V} -> V
    end.

%% @doc Set expires parameter of Contact header.
-spec set_expires({expires, expires()} | expires(), contact()) -> contact().
set_expires({expires, ExpiresVal}, #contact{} = Contact) when is_integer(ExpiresVal) ->
    set_expires(ExpiresVal, Contact);
set_expires(ExpiresVal, #contact{hparams = HParams} = Contact) when is_integer(ExpiresVal) ->
    NewHParams = ersip_hparams:set(expires, ExpiresVal, <<"expires">>, integer_to_binary(ExpiresVal), HParams),
    Contact#contact{hparams = NewHParams}.


%% @doc Get q parameter value.
-spec qvalue(contact()) -> ersip_qvalue:qvalue() | undefined.
qvalue(#contact{hparams = HParams}) ->
    case ersip_hparams:find(q, HParams) of
        not_found -> undefined;
        {ok, V} -> V
    end.

%% @doc Get q parameter value.
-spec qvalue(contact(), Default :: term()) -> ersip_qvalue:qvalue() | term().
qvalue(#contact{hparams = HParams}, Default) ->
    case ersip_hparams:find(q, HParams) of
        not_found -> Default;
        {ok, V} -> V
    end.

%% @doc Set q parameter value.
-spec set_qvalue(ersip_qvalue:qvalue(), contact()) -> contact().
set_qvalue({qvalue, _} = QVal, #contact{hparams = HParams} = Contact) ->
    NewHParams = ersip_hparams:set(q, QVal, <<"q">>, ersip_qvalue:assemble(QVal), HParams),
    Contact#contact{hparams = NewHParams}.

%% @doc Get parameter by name.
%% Example:
%% ```
%%   {ok, <<"99">>} = ersip_hdr_contact:param(<<"X">>, ersip_hdr_contact:make(<<"<sip:a@b>;x=99">>)),
%%   not_found = ersip_hdr_contact:param(<<"y">>, ersip_hdr_contact:make(<<"<sip:a@b>;x=99">>)).
%% '''
-spec param(Name :: binary(), contact()) -> {ok, Value :: binary()} | not_found.
param(Name, #contact{hparams = HParams}) when is_binary(Name) ->
    ersip_hparams:find_raw(Name, HParams).

%% @doc Set parameter by name.
%% Example
%% ```
%%   Contact = ersip_hdr_contact:make(<<"sip:a@b">>),
%%   99 = ersip_hdr_contact:expires(ersip_hdr_contact:set_param(<<"expires">>, <<"99">>, Contact), 3600).
%%   QValue = ersip_qvalue:make(<<"1">>),
%%   QValue = ersip_hdr_contact:qvalue(ersip_hdr_contact:set_param(<<"q">>, <<"1">>, Contact), 1),
%%   {ok, <<"11">>} = ersip_hdr_contact:param(<<"x">>, ersip_hdr_contact:set_param(<<"X">>, <<"11">>, Contact)).
%% '''
-spec set_param(Name :: binary(), PValue :: binary(), contact()) -> contact().
set_param(PName, PValue, #contact{hparams = HParams} = Contact)
        when is_binary(PName), is_binary(PValue) ->
    case ersip_hparams:set(PName, PValue, fun parse_known/2, HParams) of
        {ok, NewHParam} ->
            Contact#contact{hparams = NewHParam};
        {error, Reason} ->
            error(Reason)
    end.

%% @doc Get all parameters in raw representation.
%% Example
%% ```
%%   [{<<"x">>, <<"1">>}, <<"y">>] = ersip_hdr_contact:all_raw_params(ersip_hdr_contact:make(<<"sip:a@b;x=1;y">>)).
%% '''
-spec all_raw_params(contact()) -> [{binary(), binary()} | binary()].
all_raw_params(#contact{hparams = HParams}) ->
    ersip_hparams:to_raw_list(HParams).

%% @doc Create Contact header from binary or raw values.
%% Raise error if input is not well-formed Conact header.
-spec make(binary() | raw()) -> contact().
make(Bin) when is_binary(Bin) ->
    case ersip_hdr_contact:parse(Bin) of
        {ok, Contact} ->
            Contact;
        {error, Reason} ->
            error(Reason)
    end;
make(#{uri := URI} = Raw) ->
    HParams0 = ersip_hparams:make(maps:get(params, Raw, #{})),
    HParams =
        case ersip_hparams:parse_known(fun parse_known/2, HParams0) of
            {ok, H} -> H;
            {error, Reason} -> error({invalid_params, Reason})
        end,
    C0 = #contact{display_name  = ersip_display_name:make(maps:get(display_name, Raw, <<>>)),
                  uri           = ersip_uri:make(URI),
                  hparams       = HParams
                 },

    Opts = [{expires, fun(X, C) -> set_expires(X, C) end},
            {q,       fun(X, C) -> set_qvalue(ersip_qvalue:make(X), C) end}],
    ersip_map:apply_to(Opts, Raw, C0).

%% @doc Parse single contact value.
%%
%% Examples:
%% ```
%%   {ok, _} = ersip_hdr_contact:parse(<<"sip:alice@atlanta.com">>).
%%   {ok, _} = ersip_hdr_contact:parse(<<"<sip:alice@atlanta.com>">>).
%%   {ok, _} = ersip_hdr_contact:parse(<<"Alice <sip:alice@atlanta.com>">>).
%%   {ok, _} = ersip_hdr_contact:parse(<<"Alice <sip:alice@atlanta.com>;expires=30">>).
%%   {error, _} = ersip_hdr_contact:parse(<<"Alice <sip:alice@atlanta.com>;expires=30, Bob <sip;bob@biloxi.com>">>).
%% '''
-spec parse(binary()) -> parse_result().
parse(Bin) ->
    case parse_hdr(Bin) of
        {ok, Contact, <<>>} ->
            {ok, Contact};
        {ok, _, _} ->
            {error, {invalid_contact, Bin}};
        {error, _} = Error ->
            Error
    end.

%% @doc Parse contact header and return the rest.
%%
%% This function is used to parse comma-separated contact values for
%% REGISTER case.
-spec parse_hdr(binary()) -> ersip_parser_aux:parse_result(contact()).
parse_hdr(Bin) ->
    Parsers = [fun ersip_nameaddr:parse/1,
               fun ersip_parser_aux:trim_lws/1,
               fun parse_contact_params/1
              ],
    case ersip_parser_aux:parse_all(Bin, Parsers) of
        {ok, [{DisplayName, URI}, _, HParams], Rest} ->
            Contact = #contact{display_name = DisplayName,
                               uri          = URI,
                               hparams      = HParams},
            {ok, Contact, Rest};
        {error, Reason} ->
            {error, {invalid_contact, Reason}}
    end.

%% @doc Serialize header to iolist.
-spec assemble(contact()) -> iolist().
assemble(#contact{} = Contact) ->
    #contact{display_name = DN, uri = URI, hparams = HParams} = Contact,
    HParamsIO0 = ersip_hparams:assemble(HParams),
    HParamsIO =
        case ersip_iolist:is_empty(HParamsIO0) of
            true -> [];
            false -> [$; | HParamsIO0]
        end,
    [ersip_nameaddr:assemble(DN, URI), HParamsIO].

%% @doc Serialize header to binary.
-spec assemble_bin(contact()) -> binary().
assemble_bin(#contact{} = Contact) ->
    iolist_to_binary(assemble(Contact)).

%% @doc Raw (in plain erlang terms()) representation of contact header.
%%
%% Examples:
%% ```
%%   #{display_name := <<"Alice">>} = ersip_hdr_contact:raw(ersip_hdr_contact:make(<<"Alice <sip:alice@atlanta.com>;expires=20;q=0.1">>)).
%%   #{uri := #{sip := #{host := <<"atlanta.com">>}}} = ersip_hdr_contact:raw(ersip_hdr_contact:make(<<"Alice <sip:alice@atlanta.com>;expires=20;q=0.1">>)).
%% '''
-spec raw(contact()) -> raw().
raw(#contact{} = C) ->
    #contact{display_name = DN, uri = URI, hparams = HParams} = C,
    Raw = #{uri    => ersip_uri:raw(URI),
            params => ersip_hparams:raw(HParams),
            display_name => ersip_display_name:raw(DN)
           },
    Opts = [{expires,      expires(C), fun(X) -> X end},
            {q,            qvalue(C),  fun(X) -> ersip_qvalue:raw(X) end}],
    OptsKVP = [{K, F(V)} || {K, V, F} <- Opts, V /= undefined],
    maps:merge(maps:from_list(OptsKVP), Raw).

%%===================================================================
%% Internal Implementation
%%===================================================================

%% @private
-spec parse_known(binary(), binary()) -> ersip_hparams:parse_known_fun_result().
parse_known(<<"expires">>, Value) ->
    try
        case binary_to_integer(Value) of
            V when V >= 0 -> {ok, {expires, V}};
            _ -> {error, {invalid_expires, Value}}
        end
    catch
        error:badarg ->
            {error, {invalid_expires, Value}}
    end;
parse_known(<<"q">>, Value) ->
    case ersip_qvalue:parse(Value) of
        {ok, QVal} -> {ok, {q, QVal}};
        {error, Reason} ->
            {error, {invalid_qvalue, Reason}}
    end;
parse_known(_, _) ->
    {ok, unknown}.

%% @private
-spec parse_contact_params(binary()) -> ersip_parser_aux:parse_result(ersip_hparams:hparams()).
parse_contact_params(<<$;, Bin/binary>>) ->
    ersip_hparams:parse(fun parse_known/2, Bin);
parse_contact_params(Bin) ->
    {ok, ersip_hparams:new(), Bin}.
