%%
%% Copyright (c) 2017 Dmitry Poroh
%% All rights reserved.
%% Distributed under the terms of the MIT License. See the LICENSE file.
%%
%% SIP Raw header
%%

-module(ersip_hdr).

-export([ make_key/1,
          new/1,
          add_value/2,
          raw_values/1,
          as_integer/1
        ]).

-record(header, { name        :: binary(),
                  values = [] :: [ binary() ]
                }).

-type header() :: #header{}.
-export_type([header/0 ]).

%%%===================================================================
%%% API
%%%===================================================================

%% @doc Create key by the header name.
%% TODO: In future I plan to add here short names support.
-spec make_key(Name :: binary()) -> { hdr_key, binary() }.
make_key(HeaderName) ->
    { hdr_key, ersip_bin:to_lower(HeaderName) }.

%% @doc Create new headers.
-spec new(Name :: binary()) -> header().
new(Name) when is_binary(Name) ->
    #header{ name = Name }.

%% @doc Append value to list of values.
-spec add_value(Value :: iolist(), header()) -> header().
add_value(Value, #header{ values = V } = Hdr) ->
    Hdr#header{ values = V ++ [ Value ] }.

%% @doc Return raw values of the header.
-spec raw_values(header()) -> [ iolist() ].
raw_values(#header{ values = Vs }) ->
    Vs.

%% @doc Create key by the header name.
%% TODO: In future I plan to add here short names support.
-spec as_integer(header()) -> { ok, integer() } | { error, term() }.
as_integer(#header{ values = [ [ V ] ] })  when is_binary(V) ->
    case catch binary_to_integer(V) of
        Int when is_integer(Int) ->
            { ok, Int };
        _ ->
            { error, invalid_integer }
    end;
as_integer(#header{ values = [ V ] }) when is_list(V) ->
    Bin = iolist_to_binary(V),
    as_integer(#header{ values = [ [ Bin ] ] });
as_integer(#header{ values = [] }) ->
    { error, no_header };
as_integer(#header{ values = [_,_ |_] }) ->
    { error, multiple_values }.
