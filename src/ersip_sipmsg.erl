%%
%% Copyright (c) 2018 Dmitry Poroh
%% All rights reserved.
%% Distributed under the terms of the MIT License. See the LICENSE file.
%%
%% SIP message
%%
%% Contains raw message (if any) and parsed headers.
%%

-module(ersip_sipmsg).

-export([ parse/2,
          get/2,
          find/2
        ]).
-export_type([ sipmsg/0,
               known_header/0
             ]).
%%%===================================================================
%%% Types
%%%===================================================================

-record(sipmsg, { raw = undefined :: ersip_msg:message(),
                  headers = #{}   :: headers()
                }).

-type sipmsg()       :: #sipmsg{}.
-type known_header() :: ersip_siphdr:known_header().
-type headers()      :: #{ from         => ersip_hdr_fromto:fromto(),
                           to           => ersip_hdr_fromto:fromto(),
                           callid       => ersip_hdr_callid:callid(),
                           cseq         => ersip_hdr_cseq:cseq(),
                           maxforwards  => pos_integer(),
                           topmost_via  => ersip_hdr_via:via()
                         }.

%%%===================================================================
%%% API
%%%===================================================================

-spec parse(ersip_msg:message(), [ known_header() ] | all) -> Result when
      Result :: { ok, sipmsg() }
              | { error, term() }.
parse(RawMsg, all) ->
    parse(RawMsg, ersip_siphdr:all_known_headers());
parse(RawMsg, Headers) ->
    Msg = #sipmsg{ raw = RawMsg },
    lists:foldl(fun maybe_parse_header/2, { ok, Msg }, Headers).

-spec get(known_header(), sipmsg()) -> Value when
      Value :: term().
get(HdrAtom, #sipmsg{} = Msg) ->
    case find(HdrAtom, Msg) of
        { ok, Value } ->
            Value;
        not_found ->
            error({ error, { no_header, HdrAtom } });
        { error, _ } = Error ->
            error(Error)
    end.

-spec find(known_header(), sipmsg()) -> Result when
      Result :: { ok, term() }
              | not_found
              | { error, term() }.
find(HdrAtom, #sipmsg{ headers = H } = Msg) ->
    case maps:find(HdrAtom, H) of
        { ok, Value } ->
            { ok, Value };
        error ->
            case ersip_siphdr:parse_header(HdrAtom, raw_message(Msg)) of
                { ok, no_header } ->
                    not_found;
                { ok, Value } ->
                    { ok, Value };
                { error, _  } = Error ->
                    Error
            end
    end.

%%%===================================================================
%%% Internal implementation
%%%===================================================================

%%%
%%% Getters/Setters
%%%
-spec headers(sipmsg()) -> headers().
headers(#sipmsg{ headers = H }) ->
    H.

-spec set_headers(headers(), sipmsg()) -> sipmsg().
set_headers(H, #sipmsg{} = M) ->
    M#sipmsg{ headers = H }.

-spec raw_message(sipmsg()) -> ersip_msg:message().
raw_message(#sipmsg{ raw = R }) ->
    R.

%%%
%%% Parsing infrastructure
%%%
-type maybe_sipmsg() :: { ok, sipmsg() }
                      | { error, term() }.

-spec maybe_parse_header(known_header(), maybe_sipmsg()) -> maybe_sipmsg().
maybe_parse_header(_, { error, _ } = Err) ->
    Err;
maybe_parse_header(Hdr, { ok, Msg }) ->
    parse_header(Hdr, Msg).

-spec parse_header(known_header(), sipmsg()) ->  maybe_sipmsg().
parse_header(HdrAtom, Msg) when is_atom(HdrAtom) ->
    case ersip_siphdr:parse_header(HdrAtom, raw_message(Msg)) of
        { ok, no_header } ->
            { ok, Msg };
        { ok, Value } ->
            Headers = headers(Msg),
            NewHeaders = Headers#{ HdrAtom => Value },
            { ok, set_headers(NewHeaders, Msg) };
        { error, _ } = Error ->
            Error
    end.

