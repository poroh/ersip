%%
%% Copyright (c) 2018 Dmitry Poroh
%% All rights reserved.
%% Distributed under the terms of the MIT License. See the LICENSE file.
%%
%% SIP option-tag list headers:
%% Supported, Unsupported, Require, Proxy-Require
%%

-module(ersip_hdr_opttag_list).

-export([ from_list/1,
          to_list/1,
          intersect/2,
          subtract/2,
          parse/1,
          build/2,
          assemble/1
        ]).
-export_type([ option_tag_list/0 ]).

%%%===================================================================
%%% Types
%%%===================================================================

-type option_tag_list() :: { option_tag_list, gb_sets:set(ersip_option_tag:option_tag()) }.

%%%===================================================================
%%% API
%%%===================================================================

-spec from_list([ ersip_option_tag:option_tag() ]) -> option_tag_list().
from_list(OptionTagList) ->
    { option_tag_list, gb_sets:from_list(OptionTagList) }.

-spec to_list(option_tag_list()) -> [ ersip_option_tag:option_tag() ].
to_list({ option_tag_list, OptionTagSet }) ->
    gb_sets:to_list(OptionTagSet).

-spec intersect(option_tag_list(), option_tag_list()) -> option_tag_list().
intersect({ option_tag_list, S1 }, { option_tag_list, S2 }) ->
    { option_tag_list, gb_sets:intersection(S1, S2) }.

-spec subtract(option_tag_list(), option_tag_list()) -> option_tag_list().
subtract({ option_tag_list, S1 }, { option_tag_list, S2 }) ->
    { option_tag_list, gb_sets:subtract(S1, S2) }.

-spec parse(ersip_hdr:header()) -> Result when
      Result :: { ok, option_tag_list() }
              | { error, Error },
      Error :: no_header
             | { invalid_option_tag_list, term() }.
parse(Header) ->
    case ersip_hdr:raw_values(Header) of
        [] ->
            { error, no_option_tag_list };
        OptionTagList ->
            try
                L = lists:map(fun(Val) ->
                                      OptionTagBin = iolist_to_binary(Val),
                                      case ersip_option_tag:parse(OptionTagBin) of
                                          { ok, OptionTag } ->
                                              OptionTag;
                                          { error, _ } = Error ->
                                              throw(Error)
                                      end
                              end,
                              OptionTagList),
                { ok, from_list(L) }
            catch
                throw:{ error, _ } = Error ->
                    { error, { invalid_option_tag_list, Error } }
            end
    end.

-spec build(HeaderName :: binary(), option_tag_list()) -> ersip_hdr:header().
build(HdrName, { option_tag_list, _ } = OptionTagList) ->
    Hdr = ersip_hdr:new(HdrName),
    ersip_hdr:add_value([ assemble(OptionTagList) ], Hdr).

-spec assemble(option_tag_list()) ->  iolist().
assemble({option_tag_list, _ } = OptionTagList) ->
    ersip_iolist:join(<<", ">>,
                      [ ersip_option_tag:to_binary(OptionTag)
                        || OptionTag <- to_list(OptionTagList)
                      ]).
