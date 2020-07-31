%%
%% Copyright (c) 2017 Dmitry Poroh
%% All rights reserved.
%% Distributed under the terms of the MIT License. See the LICENSE file.
%%
%% SIP Raw header
%%

-module(ersip_hdr).

-export([make_key/1,
         new/1,
         new/2,
         is_empty/1,
         add_value/2,
         add_values/2,
         raw_values/1,
         name/1,
         add_topmost/2,
         replace_topmost/2,
         take_topmost/1,
         serialize_rev_iolist/2,
         as_integer/1
        ]).
-export_type([header/0,
              header_key/0]).

-include("ersip_headers.hrl").

%%%===================================================================
%%% Types
%%%===================================================================

-record(header, {name        :: binary(),
                 key         :: header_key(),
                 values = [] :: [value()]
                }).
-type value() :: iolist() | binary().

-type header() :: #header{}.
-type header_key() :: ersip_hnames:header_key().

%%%===================================================================
%%% API
%%%===================================================================

%% @doc Create key by the header name.
-spec make_key(NameOrHeader) -> header_key() when
      NameOrHeader :: ersip_hnames:name_forms()
                    | header().
make_key(#header{key = Key}) ->
    Key;
make_key(HeaderName) ->
    ersip_hnames:make_key(HeaderName).

%% @doc Create new headers.
-spec new(ersip_hnames:name_forms()) -> header().
new(Name) when is_binary(Name) ->
    Key = make_key(Name),
    #header{name = Name,
            key  = Key
           };
new(KnownHeader) when is_atom(KnownHeader)  ->
    Key = ersip_hnames:make_key(KnownHeader),
    #header{name = ersip_hnames:print_form(KnownHeader),
            key  = Key
           };
new({hdr_key, _} = HdrKey)  ->
    #header{name = ersip_hnames:print_form(HdrKey),
            key  = HdrKey
           }.

-spec new(binary(), header_key()) -> header().
new(Name, Key) ->
    #header{name = Name,
            key  = Key
           }.

-spec is_empty(header()) -> boolean().
is_empty(#header{values = []}) ->
    true;
is_empty(#header{}) ->
    false.

%% @doc Append value to list of values.
-spec add_value(value(), header()) -> header().
add_value(Value, #header{values = V} = Hdr) ->
    Hdr#header{values = V ++ [Value]}.

%% @doc Append list of values to header's list of values.
-spec add_values(Values :: [value()], header()) -> header().
add_values(Values, #header{} = Hdr) ->
    lists:foldl(fun add_value/2,
                Hdr,
                Values).

%% @doc Return raw values of the header.
-spec raw_values(header()) -> [value()].
raw_values(#header{values = Vs}) ->
    Vs.

-spec add_topmost(value(), header()) -> header().
add_topmost(Value, #header{values = V} = Hdr) ->
    Hdr#header{values = [Value | V]}.

-spec replace_topmost(value(), header()) -> header().
replace_topmost(Value, #header{values = [_|Rest]} = H) ->
    H#header{values = [Value | Rest]}.

-spec take_topmost(header()) -> Result when
      Result :: {ok, value(), header()}
              | {error, no_header}.
take_topmost(#header{values = []}) ->
    {error, no_header};
take_topmost(#header{values = [V | Rest]} = H) ->
    {ok, V, H#header{values = Rest}}.

%% @doc serialize header values in reverse iolist If Acc is not empty
%% then also adds CR LF before adding header. If header has more than
%% one value then multiple headers are added separated by CR LF.
-spec serialize_rev_iolist(header(), list()) -> list().
serialize_rev_iolist(#header{key = Key} = Header, Acc) ->
    case use_comma(Key) of
        true ->
            serialize_rev_iolist_comma_impl(ensure_raw_values(Header), Acc);
        false ->
            serialize_rev_iolist_impl(ensure_raw_values(Header), Acc)
    end.

%% @doc Get integer value from the header.
-spec as_integer(header()) -> {ok, integer()} | {error, Error} when
      Error :: invalid_integer
             | multiple_values
             | no_header.
as_integer(#header{values = [V]}) when is_binary(V) ->
    case catch binary_to_integer(V) of
        Int when is_integer(Int) ->
            {ok, Int};
        _ ->
            {error, invalid_integer}
    end;
as_integer(#header{values = [V]} = H) when is_list(V) ->
    Bin = iolist_to_binary(V),
    as_integer(H#header{values = [Bin]});
as_integer(#header{values = []}) ->
    {error, no_header};
as_integer(#header{values = [_,_ |_]}) ->
    {error, multiple_values}.

-spec name(header()) -> binary().
name(#header{name = Name}) ->
    Name.

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
serialize_rev_iolist_impl(#header{values = []}, Acc) ->
    Acc;
serialize_rev_iolist_impl(#header{name = Name, values = [V | Rest]} = H, []) ->
    serialize_rev_iolist_impl(H#header{values = Rest},
                              [V, <<": ">>, Name]);
serialize_rev_iolist_impl(#header{name = Name, values = [V | Rest]} = H, Acc) ->
    serialize_rev_iolist_impl(H#header{values = Rest},
                              [V, <<": ">>, Name, <<"\r\n">> | Acc]).

-spec serialize_rev_iolist_comma_impl(header(), list()) -> list().
serialize_rev_iolist_comma_impl(#header{values = []}, Acc) ->
    Acc;
serialize_rev_iolist_comma_impl(#header{name = Name, values = Vs}, []) ->
    rev_comma_sep_values(Vs, [<<": ">> , Name]);
serialize_rev_iolist_comma_impl(#header{name = Name, values = Vs}, Acc) ->
    rev_comma_sep_values(Vs, [<<": ">> , Name, <<"\r\n">> | Acc]).

-spec rev_comma_sep_values([value()], value()) -> iolist().
rev_comma_sep_values([LastVal], Acc) ->
    [LastVal | Acc];
rev_comma_sep_values([Val | Rest], Acc) ->
    rev_comma_sep_values(Rest, [<<", ">>, Val | Acc]).

-spec use_comma(header_key()) -> boolean().
use_comma(?ERSIPH_VIA) ->     false;
use_comma(?ERSIPH_CONTACT) -> false;
use_comma(_) ->
    true.
