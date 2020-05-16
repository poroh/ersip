%%%
%%% Copyright (c) 2018-2020 Dmitry Poroh
%%% All rights reserved.
%%% Distributed under the terms of the MIT License. See the LICENSE file.
%%%
%%% SIP method set
%%%

-module(ersip_hdr_allow).

-export([has/2,
         from_list/1,
         to_list/1,
         from_method_set/1,
         to_method_set/1,
         make/1,
         parse/1,
         build/2,
         assemble/1,
         assemble_bin/1,
         raw/1
        ]).
-export_type([allow/0, raw/0]).

%%===================================================================
%% Types
%%===================================================================

-type allow() :: {allow, ersip_method_set:set()}.
-type raw() :: ersip_method_set:raw().
-type parse_result() :: {ok, allow()} | {error, parse_error()}.
-type parse_error() :: no_allow | {invalid_allow, binary()}.

%%===================================================================
%% API
%%===================================================================

-spec has(ersip_method:method(), allow()) -> boolean().
has(M, {allow, MSet}) ->
    ersip_method_set:has(M, MSet).

-spec from_list([ersip_method:method()]) -> allow().
from_list(MethodList) ->
    {allow, ersip_method_set:new(MethodList)}.

-spec to_list(allow()) -> [ersip_method:method()].
to_list({allow, MethodSet}) ->
    ersip_method_set:to_list(MethodSet).

-spec from_method_set(ersip_method_set:set()) -> allow().
from_method_set({method_set, _} = MethodSet) ->
    {allow, MethodSet}.

-spec to_method_set(allow()) -> ersip_method_set:set().
to_method_set({allow, MethodSet}) ->
    MethodSet.

-spec make(binary() | raw()) -> allow().
make(Value) when is_binary(Value) ->
    case parse(Value) of
        {ok, Allow} -> Allow;
        {error, Error} -> error(Error)
    end;
make(RawValue) when is_list(RawValue) ->
    from_method_set(ersip_method_set:make(RawValue)).

-spec parse(ersip_hdr:header()) -> parse_result().
parse(HeaderBin) when is_binary(HeaderBin) ->
    parse_header_list([HeaderBin]);
parse(Header) ->
    case ersip_hdr:raw_values(Header) of
        [] -> {error, no_allow};
        HeaderList -> parse_header_list(HeaderList)
    end.

-spec build(HeaderName :: binary(), allow()) -> ersip_hdr:header().
build(HdrName, {allow, _} = Allow) ->
    Hdr = ersip_hdr:new(HdrName),
    ersip_hdr:add_value([assemble(Allow)], Hdr).

-spec assemble(allow()) ->  iolist().
assemble({allow, _} = Allow) ->
    ersip_iolist:join(<<", ">>,
                      [ersip_method:to_binary(Method)
                       || Method <- to_list(Allow)
                      ]).

-spec assemble_bin(allow()) -> binary().
assemble_bin(Allow) ->
    iolist_to_binary(assemble(Allow)).

-spec raw(allow()) -> raw().
raw({allow, _} = Allow) ->
    ersip_method_set:raw(to_method_set(Allow)).

%%===================================================================
%% Internal implementation
%%===================================================================

-spec parse_header_list([binary()]) -> parse_result().
parse_header_list(HeaderList) ->
    try
        MethodList0 = [binary:split(H, <<",">>, [global]) || H <- HeaderList],
        MethodList1 = lists:flatten(MethodList0),
        MethodList = [ersip_bin:trim_lws(Tag) || Tag <- MethodList1],
        L = lists:map(fun(Val) ->
                              case ersip_method:parse(iolist_to_binary(Val)) of
                                  {ok, Method, <<>>} -> Method;
                                  {ok, _, _}         -> throw({error, {invalid_method, Val}});
                                  {error, _} = Error -> throw(Error)
                              end
                      end,
                      MethodList),
        {ok, from_list(L)}
    catch
        throw:{error, _} = Error ->
            {error, {invalid_allow, Error}}
    end.
