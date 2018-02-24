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

-export([ raw_message/1,
          type/1,
          method/1,
          ruri/1,
          status/1,
          has_body/1,
          get/2,
          parse/2,
          find/2
        ]).
-export_type([ sipmsg/0,
               known_header/0
             ]).
%%%===================================================================
%%% Types
%%%===================================================================

-record(sipmsg, { raw = undefined :: ersip_msg:message(),
                  method          :: ersip_method:method(),
                  ruri            :: ersip_uri:uri() | undefined,
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

-spec raw_message(sipmsg()) -> ersip_msg:message().
raw_message(#sipmsg{ raw = R }) ->
    R.

-spec type(ersip_sipmsg:sipmsg()) -> ersip_msg:type().
type(#sipmsg{} = Msg) ->
    ersip_msg:get(type, raw_message(Msg)).

-spec method(ersip_sipmsg:sipmsg()) -> ersip_method:method().
method(#sipmsg{ method = Method }) ->
    Method.

-spec ruri(ersip_sipmsg:sipmsg()) -> ersip_uri:uri().
ruri(#sipmsg{ ruri = RURI }) ->
    RURI.

-spec status(ersip_sipmsg:sipmsg()) -> undefined | ersip_status:code().
status(#sipmsg{} = SipMsg) ->
    case type(SipMsg) of
        request ->
            undefined;
        response ->
            ersip_msg:get(status, raw_message(SipMsg))
    end.

-spec has_body(ersip_sipmsg:sipmsg()) -> boolean().
has_body(#sipmsg{} = Msg) ->
    not ersip_iolist:is_empty(ersip_msg:get(body, raw_message(Msg))).

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

-spec parse(ersip_msg:message(), [ known_header() ] | all) -> Result when
      Result :: { ok, sipmsg() }
              | { error, term() }.
parse(RawMsg, all) ->
    parse(RawMsg, ersip_siphdr:all_known_headers());
parse(RawMsg, Headers) ->
    MaybeMsg = create_from_raw(RawMsg),
    lists:foldl(fun maybe_parse_header/2, MaybeMsg, Headers).


-spec find(known_header(), sipmsg()) -> Result when
      Result :: { ok, term() }
              | not_found
              | { error, term() }.
find(HdrAtom, #sipmsg{ headers = H } = Msg) ->
    case maps:find(HdrAtom, H) of
        { ok, Value } ->
            { ok, Value };
        error ->
            case ersip_siphdr:parse_header(HdrAtom, Msg) of
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

%%%
%%% Parsing infrastructure
%%%
-spec create_from_raw(ersip_msg:message()) -> maybe_sipmsg().
create_from_raw(RawMsg) ->
    MaybeMethod = method_from_raw(RawMsg),
    MaybeRURI   = ruri_from_raw(RawMsg),
    case fold_maybes([ MaybeMethod, MaybeRURI ]) of
        [ Method, RURI ] ->
            { ok,
              #sipmsg{ method = Method,
                       ruri   = RURI,
                       raw    = RawMsg
                     }
            };
        { error, _ } = Error ->
            Error
    end.

-type maybe_sipmsg() :: { ok, sipmsg() }
                      | { error, term() }.

-spec maybe_parse_header(known_header(), maybe_sipmsg()) -> maybe_sipmsg().
maybe_parse_header(_, { error, _ } = Err) ->
    Err;
maybe_parse_header(Hdr, { ok, Msg }) ->
    parse_header(Hdr, Msg).

-spec parse_header(known_header(), sipmsg()) ->  maybe_sipmsg().
parse_header(HdrAtom, Msg) when is_atom(HdrAtom) ->
    case ersip_siphdr:parse_header(HdrAtom, Msg) of
        { ok, no_header } ->
            { ok, Msg };
        { ok, Value } ->
            Headers = headers(Msg),
            NewHeaders = Headers#{ HdrAtom => Value },
            { ok, set_headers(NewHeaders, Msg) };
        { error, Reason } ->
            { error, { header_error, { HdrAtom, Reason } } }
    end.

-spec method_from_raw(ersip_msg:message()) -> MaybeMethod when
      MaybeMethod :: { ok, ersip_method:method() }
                   | { error, { invalid_cseq, term() } }.
method_from_raw(RawMsg) ->
    case ersip_msg:get(type, RawMsg) of
        request ->
            { ok, ersip_msg:get(method, RawMsg) };
        response ->
            CSeqHdr = ersip_msg:get(<<"cseq">>, RawMsg),
            case ersip_hdr_cseq:parse(CSeqHdr) of
                { ok, CSeq } ->
                    { ok, ersip_hdr_cseq:method(CSeq) };
                { error, Reason } ->
                    { error, { invalid_cseq, Reason } }
            end
    end.

-spec ruri_from_raw(ersip_msg:message()) -> MaybeRURI when
      MaybeRURI :: { ok, ersip_uri:uri() | undefined }
                   | { error, { invalid_ruri, term() } }.
ruri_from_raw(Msg) ->
    case ersip_msg:get(type, Msg) of
        request ->
            URIBin = ersip_msg:get(ruri, Msg),
            case ersip_uri:parse(URIBin) of
                { ok, _ } = R ->
                    R;
                { error, Reason } ->
                    { error, { invalid_ruri, Reason } }
            end;
        response ->
            { ok, undefined }
    end.
 
fold_maybes(MaybesList) ->
    lists:foldr(fun(_, {error, _} = Error) ->
                        Error;
                   ({ok, Result }, Acc) ->
                        [ Result | Acc ];
                   ({error, _ } = Error, _) ->
                        Error
                end,
                [],
                MaybesList).
