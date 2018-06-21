%%
%% Copyright (c) 2018 Dmitry Poroh
%% All rights reserved.
%% Distributed under the terms of the MIT License. See the LICENSE file.
%%
%% SIP qvalue
%% (used in Accept and Contact headers)
%%

-module(ersip_qvalue).

-export([make/1,
         parse/1,
         assemble/1
        ]).

-export_type([qvalue/0]).

%%%===================================================================
%%% Types
%%%===================================================================

-type qvalue() :: {qvalue, 0..1000}.

%%%===================================================================
%%% API
%%%===================================================================

-spec make(binary()) -> qvalue().
make(Bin) ->
    case parse(Bin) of
        {ok, QValue} ->
            QValue;
        {error, Reason} ->
            error(Reason)
    end.

%% qvalue         =  ( "0" [ "." 0*3DIGIT ] )
%%                  / ( "1" [ "." 0*3("0") ] )
-spec parse(binary()) -> {ok, qvalue()} | {error, term()}.
parse(Bin) ->
    parse_impl(Bin).

-spec assemble(qvalue()) -> binary().
assemble({qvalue, 1000}) ->
    <<"1">>;
assemble({qvalue, Value}) ->
    ValueBin = integer_to_binary(Value),
    case byte_size(ValueBin) of
        1 -> <<"0.00", ValueBin/binary>>;
        2 -> <<"0.0", ValueBin/binary>>;
        3 -> <<"0.", ValueBin/binary>>
    end.

%%%===================================================================
%%% Internal implementation
%%%===================================================================

%% qvalue         =  ( "0" [ "." 0*3DIGIT ] )
%%                  / ( "1" [ "." 0*3("0") ] )
-spec parse_impl(binary()) -> {ok, qvalue()} | {error, term()}.
parse_impl(<<"0">>) ->
    {ok, {qvalue, 0}};
parse_impl(<<"1">>) ->
    {ok, {qvalue, 1000}};
parse_impl(<<"1.", AllZeroes/binary>> = QVal) ->
    case all_zeroes(AllZeroes) of
        true ->
            {ok, {qvalue, 1000}};
        false ->
            {error, {invalid_qvalue, QVal}}
    end;
parse_impl(<<"0.", Rest/binary>> = QVal) when byte_size(Rest) =< 3 ->
    ValueBin = add_zeroes(Rest),
    try
        {ok, {qvalue, binary_to_integer(ValueBin)}}
    catch
        error:badarg ->
            {error, {invalid_qvalue, QVal}}
    end;
parse_impl(QVal) ->
    {error, {invalid_qvalue, QVal}}.

-spec all_zeroes(binary()) -> boolean().
all_zeroes(<<>>) ->
    true;
all_zeroes(<<"0", Rest/binary>>) ->
    all_zeroes(Rest);
all_zeroes(_) ->
    false.

-spec add_zeroes(binary()) -> binary().
add_zeroes(X) when byte_size(X) == 3 -> X;
add_zeroes(X) when byte_size(X) == 2 -> <<X/binary, "0">>;
add_zeroes(X) when byte_size(X) == 1 -> <<X/binary, "00">>;
add_zeroes(X) when byte_size(X) == 0 -> <<"000">>.

