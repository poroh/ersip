%%
%% Copyright (c) 2018 Dmitry Poroh
%% All rights reserved.
%% Distributed under the terms of the MIT License. See the LICENSE file.
%%
%% SIP One SIP Contact entry
%%

-module(ersip_hdr_contact).

-export([new/1,
         uri/1,
         expires/2,
         set_expires/2,
         qvalue/2,
         set_qvalue/2,
         param/2,
         set_param/3,
         make/1,
         parse/1,
         parse_hdr/1,
         assemble/1,
         assemble_bin/1
        ]).
-export_type([contact/0,
              contact_param/0]).

%%%===================================================================
%%% Types
%%%===================================================================

-record(contact, {display_name  :: undefined | ersip_nameaddr:display_name(),
                  uri           :: ersip_uri:uri(),
                  hparams       :: ersip_hparams:hparams()
                 }).
-type contact() :: #contact{}.

-type contact_param() :: {qvalue, ersip_qvalue:qvalue()}
                       | {expires, expires()}
                       | {binary(), binary()}.
-type expires() :: non_neg_integer().

-type parse_result() :: {ok, contact()}
                      | {error, {invalid_contact, term()}}.
-type known_param() :: q | expires.

%%%===================================================================
%%% API
%%%===================================================================

-spec new(ersip_uri:uri()) -> contact().
new(URI) ->
    #contact{uri = URI,
             hparams = ersip_hparams:new()}.

-spec uri(contact()) -> ersip_uri:uri().
uri(#contact{uri = URI}) ->
    URI.

-spec expires(contact(), Default :: non_neg_integer()) -> non_neg_integer().
expires(#contact{hparams = HParams}, Default) ->
    case ersip_hparams:find(expires, HParams) of
        not_found ->
            Default;
        {ok, V} ->
            V
    end.

-spec set_expires({expires, ExpiresVal} | ExpiresVal, contact()) -> contact() when
      ExpiresVal :: non_neg_integer().
set_expires({expires, ExpiresVal}, #contact{} = Contact) when is_integer(ExpiresVal) ->
    set_expires(ExpiresVal, Contact);
set_expires(ExpiresVal, #contact{hparams = HParams} = Contact) when is_integer(ExpiresVal) ->
    NewHParams = ersip_hparams:set(expires, ExpiresVal, <<"expires">>, integer_to_binary(ExpiresVal), HParams),
    Contact#contact{hparams = NewHParams}.

-spec qvalue(contact(), Default :: term()) -> ersip_qvalue:qvalue() | term().
qvalue(#contact{hparams = HParams}, Default) ->
    case ersip_hparams:find(q, HParams) of
        not_found ->
            Default;
        {ok, V} ->
            V
    end.

-spec set_qvalue(ersip_qvalue:qvalue(), contact()) -> contact().
set_qvalue({qvalue, _} = QVal, #contact{hparams = HParams} = Contact) ->
    NewHParams = ersip_hparams:set(q, QVal, <<"q">>, ersip_qvalue:assemble(QVal), HParams),
    Contact#contact{hparams = NewHParams}.


-spec param(Name :: binary(), contact()) -> {ok, Value :: binary()} | not_found.
param(Name, #contact{hparams = HParams}) when is_binary(Name) ->
    ersip_hparams:find_raw(Name, HParams).

-spec set_param(Name :: binary(), PValue :: binary(), contact()) -> contact().
set_param(PName, PValue, #contact{hparams = HParams} = Contact)
        when is_binary(PName), is_binary(PValue) ->
    case set_hparam(PName, PValue, HParams) of
        {ok, NewHParam} ->
            Contact#contact{hparams = NewHParam};
        {error, Reason} ->
            error(Reason)
    end.

-spec make(binary()) -> contact().
make(Bin) when is_binary(Bin) ->
    case ersip_hdr_contact:parse(Bin) of
        {ok, Contact} ->
            Contact;
        {error, Reason} ->
            error(Reason)
    end.

-spec parse(binary()) -> parse_result().
parse(Bin) ->
    case parse_hdr(Bin) of
        {ok, Contact, <<>>} ->
            {ok, Contact};
        {ok, _, _} ->
            {error, {invalid_contact, Bin}};
        {error, _} = Err ->
            Err
    end.

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

-spec assemble(contact()) -> iolist().
assemble(#contact{} = Contact) ->
    #contact{display_name = DN,
           uri = URI,
           hparams = HParams
          } = Contact,
    DisplayName = case DN of
                      undefined -> {display_name, []};
                      _ -> DN
                  end,
    HParamsIO0 = ersip_hparams:assemble(HParams),
    HParamsIO =
        case ersip_iolist:is_empty(HParamsIO0) of
            true -> [];
            false -> [$; | HParamsIO0]
        end,
    [ersip_nameaddr:assemble(DisplayName, URI), HParamsIO].

-spec assemble_bin(contact()) -> binary().
assemble_bin(#contact{} = Contact) ->
    iolist_to_binary(assemble(Contact)).

%%%===================================================================
%%% Internal Implementation
%%%===================================================================

-spec param_name_to_atom(binary()) -> {ok, known_param()} | not_found | {error, {invalid_param, binary()}}.
param_name_to_atom(<<"expires">>) -> {ok, expires};
param_name_to_atom(<<"q">>)       -> {ok, q};
param_name_to_atom(X) when is_binary(X) ->
    case ersip_parser_aux:check_token(X) of
        true -> not_found;
        false -> {error, {invalid_param, X}}
    end.

-spec parse_contact_params(binary()) -> ersip_parser_aux:parse_result(ersip_hparams:hparams()).
parse_contact_params(<<$;, Bin/binary>>) ->
    do_parse_contact_params(Bin);
parse_contact_params(Bin) ->
    {ok, ersip_hparams:new(), Bin}.

-spec do_parse_contact_params(binary()) -> ersip_parser_aux:parse_result(ersip_hparams:hparams()).
do_parse_contact_params(Bin) ->
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


-spec parse_param(known_param(), binary()) -> {ok, Value} | {error, Err} when
      Value :: {expires, integer()}
             | {q, ersip_qvalue:qvalue()},
      Err   :: {invalid_expires, binary()}
             | {invalid_qvalue, binary()}.
parse_param(expires, Value) ->
    try
        {ok, binary_to_integer(Value)}
    catch
        error:badarg ->
            {error, {invalid_expires, Value}}
    end;
parse_param(q, Value) ->
    case ersip_qvalue:parse(Value) of
        {ok, _} = Ok -> Ok;
        {error, Reason} ->
            {error, {invalid_qvalue, Reason}}
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
