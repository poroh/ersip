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
          add_values/2,
          raw_values/1,
          serialize_rev_iolist/2,
          as_integer/1
        ]).

-record(header, { name        :: binary(),
                  values = [] :: [ iolist() ]
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

%% @doc Append list of values to headr's list of values.
-spec add_values(Value :: [ iolist() ], header()) -> header().
add_values(Values, #header{ values = V } = Hdr) ->
    Hdr#header{ values = V ++ Values }.

%% @doc Return raw values of the header.
-spec raw_values(header()) -> [ iolist() ].
raw_values(#header{ values = Vs }) ->
    Vs.

%% @doc serialize header values in reverse iolist If Acc is not empty
%% then also adds CR LF before adding header. If header has more than
%% one value then multiple headers are added separated by CR LF.
-spec serialize_rev_iolist(header(), list()) -> list().
serialize_rev_iolist(#header{} = Header, Acc) ->
    serialize_rev_iolist_impl(ensure_raw_values(Header), Acc).

%% @doc Get integer value from the header.
-spec as_integer(header()) -> { ok, integer() } | { error, term() }.
as_integer(#header{ values = [ [ V ] ] })  when is_binary(V) ->
    case catch binary_to_integer(V) of
        Int when is_integer(Int) ->
            { ok, Int };
        _ ->
            { error, invalid_integer }
    end;
as_integer(#header{ values = [ V ] } = H) when is_list(V) ->
    Bin = iolist_to_binary(V),
    as_integer(H#header{ values = [ [ Bin ] ] });
as_integer(#header{ values = [] }) ->
    { error, no_header };
as_integer(#header{ values = [_,_ |_] }) ->
    { error, multiple_values }.

%%%===================================================================
%%% Internal implementation
%%%===================================================================

%% @private
%% @doc Ensure that header has raw values (serializable)
-spec ensure_raw_values(header()) -> header().
ensure_raw_values(Header) ->
    Header.

%% @private
%% @doc Create reverse iolist value of the header
-spec serialize_rev_iolist_impl(header(), list()) -> list().
serialize_rev_iolist_impl(#header{ values = [] }, Acc) ->
    Acc;
serialize_rev_iolist_impl(#header{ name = Name, values = [V | Rest] } = H, []) ->
    serialize_rev_iolist_impl(H#header{ values = Rest },
                              [ V, <<": ">>, Name ]);
serialize_rev_iolist_impl(#header{ name = Name, values = [V | Rest] } = H, Acc) ->
    serialize_rev_iolist_impl(H#header{ values = Rest },
                              [ V, <<": ">>, Name, <<"\r\n">> | Acc ]).
