%%%
%%% Copyright (c) 2021 Dmitry Poroh
%%% All rights reserved.
%%% Distributed under the terms of the MIT License. See the LICENSE file.
%%%
%%% TEL URI (RFC3966)
%%%

-module(ersip_uri_tel).

-export([type/1,
         phone/1,
         extension/1,
         phone_context_type/1,
         phone_context/1,

         make/1,
         parse/1,
         assemble/1,
         assemble_bin/1,
         make_key/1
        ]).

-export_type([tel_uri/0,
              parse_error/0
             ]).

%%===================================================================
%% Types
%%===================================================================

-record(tel_uri, {subscriber  :: tel_subscriber(),
                  params      :: [{binary(), binary()}]
                 }).
-type tel_uri() :: #tel_uri{}.

-type tel_subscriber() :: {global, binary()}
                        | {local, binary()}.

-type parse_result() :: {ok, tel_uri()} | {error, parse_error()}.
-type parse_error()  :: {invalid_phone_context, binary()}
                      | {invalid_scheme, binary()}
                      | {garbage_at_the_end, binary()}.
-type number_end_state() :: {non_neg_integer(), non_neg_integer()|undefined}.

%%===================================================================
%% API
%%===================================================================

-spec type(tel_uri()) -> global | local.
type(#tel_uri{subscriber = {T, _}}) ->
    T.

-spec phone(tel_uri()) -> binary().
phone(#tel_uri{subscriber = {_, Number}}) ->
    Number.

-spec extension(tel_uri()) -> binary() | undefined.
extension(#tel_uri{params = P}) ->
    proplists:get_value(<<"ext">>, P).

-spec phone_context_type(tel_uri()) -> undefined | domain | phone_prefix.
phone_context_type(#tel_uri{params = P}) ->
    case proplists:get_value(<<"phone-context">>, P) of
        undefined -> undefined;
        <<"+", _/binary>> -> phone_prefix;
        _ -> domain
    end.

-spec phone_context(tel_uri()) -> ersip_host:host() | binary() | undefined.
phone_context(#tel_uri{params = P}) ->
    case proplists:get_value(<<"phone-context">>, P) of
        undefined -> undefined;
        <<"+", _/binary>> = X -> X;
        X ->
            {ok, H, <<>>} = ersip_host:parse(X),
            H
    end.

-spec make(binary()) -> tel_uri().
make(Bin) ->
    case parse(Bin) of
        {ok, TelURI} ->
            TelURI;
        {error, Reason} ->
            error({invalid_tel_uri, Reason})
    end.

-spec parse(binary()) -> parse_result().
parse(Bin) ->
    case ersip_uri_parser_aux:split_scheme(Bin) of
        {<<"tel">>, Rest} ->
            parse_data(Rest);
        {Scheme, _} ->
            {error, {invalid_scheme, Scheme}}
    end.

%% @private
-spec assemble(tel_uri()) -> iolist().
assemble(#tel_uri{subscriber = {_, T}, params = P}) ->
    [<<"tel:">>, T,
     case P == [] of
         true -> [];
         false ->
            [$;,
             ersip_iolist:join(
               <<";">>,
               lists:map(fun ({Name, <<>>}) ->
                                 Name;
                             ({Name, Value}) ->
                                 [Name, $=, Value]
                         end,
                         P))
            ]
     end
    ].

-spec assemble_bin(tel_uri()) -> binary().
assemble_bin(#tel_uri{} = URI) ->
    iolist_to_binary(assemble(URI)).

%% Two "tel" URIs are equivalent according to the following rules:
%%
%% o  Both must be either a 'local-number' or a 'global-number', i.e.,
%%    start with a '+'.
%% o  The 'global-number-digits' and the 'local-number-digits' must be
%%    equal, after removing all visual separators.
%% o  For mandatory additional parameters (section 5.4) and the 'phone-
%%    context' and 'extension' parameters defined in this document, the
%%    'phone-context' parameter value is compared as a host name if it
%%    is a 'domainname' or digit by digit if it is 'global-number-
%%    digits'.  The latter is compared after removing all 'visual-
%%    separator' characters.
%% o  Parameters are compared according to 'pname', regardless of the
%%    order they appeared in the URI.  If one URI has a parameter name
%%    not found in the other, the two URIs are not equal.
%% o  URI comparisons are case-insensitive.
-spec make_key(tel_uri()) -> tel_uri().
make_key(#tel_uri{subscriber = S, params = P}) ->
    #tel_uri{subscriber = subscriber_key(S),
             params = parameters_key(P)
            }.

%%===================================================================
%% Implementation
%%===================================================================

-include("ersip_abnf.hrl").

%% @doc Parse TEL URI data (without scheme).
%% @private
-spec parse_data(binary()) -> parse_result().
parse_data(Bin) ->
    Parsers = [fun parse_subscriber/1,
               fun ersip_parser_aux:trim_lws/1,
               fun parse_params/1
              ],
    case ersip_parser_aux:parse_all(Bin, Parsers) of
        {ok, [Subscriber, _, Params], <<>>} ->
             TelURI = #tel_uri{subscriber = Subscriber,
                               params     = Params},
            {ok, TelURI};
        {ok, _, Rest} -> {error, {garbage_at_the_end, Rest}};
        {error, _} = Err -> Err
    end.

-spec parse_subscriber(binary()) -> ersip_parser_aux:parse_result(tel_subscriber()).
parse_subscriber(<<"+", Number/binary>> = V) ->
    case find_global_number_end(Number) of
        {0, undefined} -> {error, {invalid_subscriber, V}};
        {_, undefined} -> {error, {invalid_subscriber, V}};
        {N, _} ->
            N1 = N+1,
            <<SubscriberBin:N1/binary, Rest/binary>> = V,
            {ok, {global, SubscriberBin}, Rest}
    end;
parse_subscriber(Number) ->
    case find_local_number_end(Number) of
        {0, undefined} -> {error, {invalid_subscriber, Number}};
        {_, undefined} -> {error, {invalid_subscriber, Number}};
        {N, _} ->
            <<SubscriberBin:N/binary, Rest/binary>> = Number,
            {ok, {local, SubscriberBin}, Rest}
    end.

%% visual-separator     = "-" / "." / "(" / ")"
-define(is_visual_separator(X), (X == $- orelse X == $. orelse X == $( orelse X == $))).

%% global-number-digits = "+" *phonedigit DIGIT *phonedigit
%% phonedigit           = DIGIT / [ visual-separator ]
-spec find_global_number_end(binary()) -> number_end_state().
find_global_number_end(Bin) ->
    do_find_global_number_end(Bin, {0, undefined}).


-spec do_find_global_number_end(binary(), number_end_state()) -> number_end_state().
do_find_global_number_end(<<X:8, Rest/binary>>, {Pos, _}) when ?is_DIGIT(X) ->
    do_find_global_number_end(Rest, {Pos+1, Pos});
do_find_global_number_end(<<X:8, Rest/binary>>, {Pos, DigPos}) when ?is_visual_separator(X) ->
    do_find_global_number_end(Rest, {Pos+1, DigPos});
do_find_global_number_end(_, Acc) ->
    Acc.

%% local-number-digits  = *phonedigit-hex (HEXDIG / "*" / "#") *phonedigit-hex
%% phonedigit-hex       = HEXDIG / "*" / "#" / [ visual-separator ]
-spec find_local_number_end(binary()) -> number_end_state().
find_local_number_end(Bin) ->
    do_find_local_number_end(Bin, {0, undefined}).

-spec do_find_local_number_end(binary(), number_end_state()) -> number_end_state().
do_find_local_number_end(<<X:8, Rest/binary>>, {Pos, _}) when ?is_HEXDIG(X); X == $*; X == $# ->
    do_find_local_number_end(Rest, {Pos+1, Pos});
do_find_local_number_end(<<X:8, Rest/binary>>, {Pos, DigPos}) when ?is_visual_separator(X) ->
    do_find_local_number_end(Rest, {Pos+1, DigPos});
do_find_local_number_end(_, Acc) ->
    Acc.

-spec parse_params(binary()) -> parse_result().
parse_params(<<";", Rest/binary>>) ->
    ersip_parser_aux:parse_kvps(fun tel_uri_header_validator/2, <<";">>, Rest);
parse_params(Other) ->
    {ok, [], Other}.

-spec tel_uri_header_validator(binary(), binary() | novalue) -> 
                                      {ok, {binary(), binary()}}
                                    | {error, parse_error()}.
tel_uri_header_validator(Key, novalue) ->
    {ok, {Key, <<>>}};
tel_uri_header_validator(<<"phone-context">> = Key, Value) ->
    case is_valid_phone_context(Value) of
        true -> {ok, {Key, Value}};
        false -> {error, {invalid_phone_context, Value}}
    end;
tel_uri_header_validator(Key, Value) ->
    {ok, {Key, Value}}.


%% Two "tel" URIs are equivalent according to the following rules:
%%
%% o  Both must be either a 'local-number' or a 'global-number', i.e.,
%%    start with a '+'.
%% o  The 'global-number-digits' and the 'local-number-digits' must be
%%    equal, after removing all visual separators.
-spec subscriber_key(tel_subscriber()) -> tel_subscriber().
subscriber_key({Type, Digits}) ->
    {Type, ersip_bin:to_lower(remove_visual_separtors(Digits))}.


%% o  For mandatory additional parameters (section 5.4) and the 'phone-
%%    context' and 'extension' parameters defined in this document, the
%%    'phone-context' parameter value is compared as a host name if it
%%    is a 'domainname' or digit by digit if it is 'global-number-
%%    digits'.  The latter is compared after removing all 'visual-
%%    separator' characters.
%% o  Parameters are compared according to 'pname', regardless of the
%%    order they appeared in the URI.  If one URI has a parameter name
%%    not found in the other, the two URIs are not equal.
%% o  URI comparisons are case-insensitive.
-spec parameters_key([{binary(), binary()}]) -> [{binary(), binary()}].
parameters_key(P) ->
    P1 = [case ersip_bin:to_lower(K) of
              <<"phone-context">> = KLower ->
                  case V of
                      <<"+", _/binary>> ->
                          {KLower, ersip_bin:to_lower(remove_visual_separtors(V))};
                      _ ->
                          {ok, H, <<>>} = ersip_host:parse(V),
                          {KLower, ersip_host:assemble_bin(ersip_host:make_key(H))}
                  end;
              <<"ext">> = KLower ->
                  {KLower, ersip_bin:to_lower(remove_visual_separtors(V))};
              KLower ->
                  {KLower, ersip_bin:to_lower(ersip_uri_parser_aux:unquote_hex(V))}
          end || {K, V} <- P],
    lists:sort(P1).

-spec remove_visual_separtors(binary()) -> binary().
remove_visual_separtors(Phone) ->
    iolist_to_binary([X || X <- binary_to_list(Phone), not ?is_visual_separator(X)]).


%% context              = ";phone-context=" descriptor
%% descriptor           = domainname / global-number-digits
-spec is_valid_phone_context(binary()) -> boolean().
is_valid_phone_context(<<"+", Rest/binary>>) ->
    RestLen = byte_size(Rest),
    case find_global_number_end(Rest) of
        {_, undefined} -> false;
        {RestLen, _}   -> true;
        {_, _}         -> false
    end;
is_valid_phone_context(Value) ->
    case ersip_host:parse(Value) of
        {ok, _, <<>>} -> true;
        {ok, _, _} -> false;
        {error, _} -> false
    end.

