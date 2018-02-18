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
-export_type([ header/0 ]).

%%%===================================================================
%%% Types
%%%===================================================================

-record(header, { name        :: binary(),
                  key         :: header_key(),
                  values = [] :: [ iolist() | binary() ]
                }).

-type header() :: #header{}.
-type header_key() :: { hdr_key, binary() }.

%%%===================================================================
%%% API
%%%===================================================================

%% @doc Create key by the header name.
-spec make_key(NameOrHeader) -> header_key() when
      NameOrHeader :: binary() | header().
make_key(#header{ key = Key }) ->
    Key;
make_key(HeaderName) ->
    { hdr_key, ersip_hdr_names:compact_form(HeaderName) }.

%% @doc Create new headers.
-spec new(Name :: binary()) -> header().
new(Name) when is_binary(Name) ->
    #header{ name = Name,
             key  = make_key(Name)
           }.

%% @doc Append value to list of values.
-spec add_value(Value :: iolist(), header()) -> header().
add_value(Value, #header{ values = V, key = Key } = Hdr) ->
    Values = comma_split(Key, Value),
    Hdr#header{ values = V ++ Values }.

%% @doc Append list of values to headr's list of values.
-spec add_values(Value :: [ iolist() ], header()) -> header().
add_values(Values, #header{} = Hdr) ->
    lists:foldl(fun add_value/2,
                Hdr,
                Values).

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
-spec as_integer(header()) -> { ok, integer() } | { error, Error } when
      Error :: invalid_integer
             | multiple_values
             | no_header.
as_integer(#header{ values = [ V ] }) when is_binary(V) ->
    case catch binary_to_integer(V) of
        Int when is_integer(Int) ->
            { ok, Int };
        _ ->
            { error, invalid_integer }
    end;
as_integer(#header{ values = [ V ] } = H) when is_list(V) ->
    Bin = iolist_to_binary(V),
    as_integer(H#header{ values = [ Bin ] });
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

%% @private
%% @doc split headers with comma:
%% RFC 3261 7.3.1 Header Field Format
%% It MUST be possible to combine the multiple
%% header field rows into one "field-name: field-value" pair, without
%% changing the semantics of the message, by appending each subsequent
%% field-value to the first, each separated by a comma.  The exceptions
%% to this rule are the WWW-Authenticate, Authorization, Proxy-
%% Authenticate, and Proxy-Authorization header fields.
-spec comma_split(header_key(), iolist()) -> [ iolist() ].
comma_split({ hdr_key, <<"www-authenticate">> }, V) ->
    [ ersip_iolist:trim_lws(V) ];
comma_split({ hdr_key, <<"authorization">> }, V) ->
    [ ersip_iolist:trim_lws(V) ];
comma_split({ hdr_key, <<"proxy-authenticate">> }, V) ->
    [ ersip_iolist:trim_lws(V) ];
comma_split({ hdr_key, <<"proxy-authorization">> }, V) ->
    [ ersip_iolist:trim_lws(V) ];
comma_split(_, V) ->
    Bin = iolist_to_binary(V),
    lists:map(fun ersip_bin:trim_lws/1,
              binary:split(Bin, <<",">>, [ global ])).
