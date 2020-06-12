%%%
%%% Copyright (c) 2018, 2020 Dmitry Poroh
%%% All rights reserved.
%%% Distributed under the terms of the MIT License. See the LICENSE file.
%%%
%%% SIP option-tag list headers:
%%% Supported, Unsupported, Require, Proxy-Require
%%%

-module(ersip_hdr_opttag_list).

-export([make/1,
         from_list/1,
         to_list/1,
         intersect/2,
         subtract/2,
         append/2,
         parse/1,
         build/2,
         assemble/1,
         assemble_bin/1,
         raw/1
        ]).
-export_type([option_tag_list/0]).

%%===================================================================
%% Types
%===================================================================

-type option_tag_list() :: {option_tag_list, gb_sets:set(ersip_option_tag:option_tag())}.
-type parse_result() :: {ok, option_tag_list()} | {error, parse_error()}.
-type parse_error() :: no_header
                     | {invalid_option_tag_list, term()}.
-type raw() :: [binary()].

%%===================================================================
%% API
%%===================================================================

%% @doc Make option tags from binary() or raw representation.
-spec make(binary() | raw()) -> option_tag_list().
make(Bin) when is_binary(Bin) ->
    case parse_tag_list([Bin]) of
        {ok, TagList}  -> TagList;
        {error, Reason} -> error(Reason)
    end;
make(List) when is_list(List) ->
    case parse_tag_list(List) of
        {ok, TagList}  -> TagList;
        {error, Reason} -> error(Reason)
    end.

%% @doc Create from list of option tags.
-spec from_list([ersip_option_tag:option_tag()]) -> option_tag_list().
from_list(OptionTagList) ->
    {option_tag_list, gb_sets:from_list(OptionTagList)}.

%% @doc Save to list of option tags.
-spec to_list(option_tag_list()) -> [ersip_option_tag:option_tag()].
to_list({option_tag_list, OptionTagSet}) ->
    gb_sets:to_list(OptionTagSet).

%% @doc Create intersection of sets of option tags.
-spec intersect(option_tag_list(), option_tag_list()) -> option_tag_list().
intersect({option_tag_list, S1}, {option_tag_list, S2}) ->
    {option_tag_list, gb_sets:intersection(S1, S2)}.

%% @doc Create subtractions of sets of option tags.
-spec subtract(option_tag_list(), option_tag_list()) -> option_tag_list().
subtract({option_tag_list, S1}, {option_tag_list, S2}) ->
    {option_tag_list, gb_sets:subtract(S1, S2)}.

%% @doc Append option tag to option tags.
-spec append(ersip_option_tag:option_tag(), option_tag_list()) -> option_tag_list().
append({option_tag, _} = OptionTag, {option_tag_list, S}) ->
    {option_tag_list, gb_sets:insert(OptionTag, S)}.

%% @doc Parse options tags from binary or raw SIP header.
-spec parse(binary() | ersip_hdr:header()) -> parse_result().
parse(Binary) when is_binary(Binary) ->
    parse_tag_list([Binary]);
parse(Header) ->
    case ersip_hdr:raw_values(Header) of
        [] ->
            {error, no_option_tag_list};
        HeaderList ->
            parse_tag_list(HeaderList)
    end.

%% @doc Build raw SIP header from options tags list.
-spec build(HeaderName :: binary(), option_tag_list()) -> ersip_hdr:header().
build(HdrName, {option_tag_list, _} = OptionTagList) ->
    Hdr = ersip_hdr:new(HdrName),
    ersip_hdr:add_value([assemble(OptionTagList)], Hdr).

%% @doc Assemble options tags list to iolist
-spec assemble(option_tag_list()) ->  iolist().
assemble({option_tag_list, _} = OptionTagList) ->
    ersip_iolist:join(<<", ">>,
                      [ersip_option_tag:to_binary(OptionTag)
                       || OptionTag <- to_list(OptionTagList)
                      ]).

%% @doc Assemble options tags list to binary
-spec assemble_bin(option_tag_list()) -> binary().
assemble_bin({option_tag_list, _} = OptionTagList) ->
    iolist_to_binary(assemble(OptionTagList)).

%% @doc Raw representation of option tags list.
-spec raw(option_tag_list()) -> raw().
raw({option_tag_list, _} = OTL) ->
    [ersip_option_tag:to_binary(OT) || OT <- to_list(OTL)].

%%===================================================================
%% Internal implementation
%%===================================================================

-spec parse_tag_list([iolist() | binary()]) -> parse_result().
parse_tag_list(Headers) ->
    AllTagsBin = iolist_to_binary(ersip_iolist:join($,, Headers)),
    AllTags0 = binary:split(AllTagsBin, <<",">>, [global]),
    OptionTagList = [ersip_bin:trim_lws(Tag) || Tag <- AllTags0],
    try
        L = lists:map(fun(Val) ->
                              OptionTagBin = iolist_to_binary(Val),
                              case ersip_option_tag:parse(OptionTagBin) of
                                  {ok, OptionTag} ->
                                      OptionTag;
                                  {error, _} = Error ->
                                      throw(Error)
                              end
                      end,
                      OptionTagList),
        {ok, from_list(L)}
    catch
        throw:{error, X} -> {error, {invalid_option_tag_list, X}}
    end.
