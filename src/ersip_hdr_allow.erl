%%
%% Copyright (c) 2018 Dmitry Poroh
%% All rights reserved.
%% Distributed under the terms of the MIT License. See the LICENSE file.
%%
%% SIP method set
%%

-module(ersip_hdr_allow).

-export([ from_list/1,
          to_list/1,
          parse/1,
          build/2,
          assemble/1
        ]).
-export_type([ allow/0 ]).

%%%===================================================================
%%% Types
%%%===================================================================

-type allow() :: { allow, gb_sets:item(ersip_method:method()) }.

%%%===================================================================
%%% API
%%%===================================================================

-spec from_list([ ersip_method:method() ]) -> allow().
from_list(MethodList) ->
    { allow, gb_sets:from_list(MethodList) }.

-spec to_list(allow()) -> [ ersip_method:method() ].
to_list({ allow, MethodSet }) ->
    gb_sets:to_list(MethodSet).

-spec parse(ersip_hdr:header()) -> Result when
      Result :: { ok, allow() }
              | { error, Error },
      Error :: no_allow
             | { invalid_allow, binary() }.
parse(Header) ->
    case ersip_hdr:raw_values(Header) of
        [] ->
            { error, no_allow };
        Allows ->
            try
                L = lists:map(fun(Val) ->
                                      case ersip_method:parse(iolist_to_binary(Val)) of
                                          { ok, Method }->
                                              Method;
                                          { error, _ } = Error ->
                                              throw(Error)
                                      end
                              end,
                              Allows),
                { ok, from_list(L) }
            catch
                throw:{ error, _ } = Error ->
                    { error, { invalid_allow, Error } }
            end
    end.

-spec build(HeaderName :: binary(), allow()) -> ersip_hdr:header().
build(HdrName, { allow, _ } = Allow) ->
    Hdr = ersip_hdr:new(HdrName),
    ersip_hdr:add_value([ assemble(Allow) ], Hdr).

-spec assemble(allow()) ->  binary().
assemble({ allow, _ } = Allow) ->
    ersip_iolist:join(<<", ">>,
                      [ ersip_method:to_binary(Method)
                        || Method <- to_list(Allow)
                      ]).
