%%
%% Copyright (c) 2018 Dmitry Poroh
%% All rights reserved.
%% Distributed under the terms of the MIT License. See the LICENSE file.
%%
%% SIP Headers Helpers
%%

-module(ersip_siphdr).

-export([ all_known_headers/0,
          parse_header/2
        ]).
-export_type([ known_header/0 ]).

%%%===================================================================
%%% Types
%%%===================================================================

-type known_header() :: from
                      | to
                      | callid
                      | cseq
                      | maxforwards
                      | topmost_via
                      | content_type.

-record(descr, { name      :: binary(),
                 required  :: header_required(),
                 parse_fun :: parse_fun_type() }).
-type descr() :: #descr{}.

-type parse_fun_type() :: fun((ersip_hdr:header()) -> parse_fun_result()).
-type parse_fun_result() :: { ok, Value :: term() }
                          | { error, Reason :: term() }.

%%%===================================================================
%%% API
%%%===================================================================

all_known_headers() ->
    [ from,
      to,
      callid,
      cseq,
      maxforwards,
      topmost_via,
      content_type
    ].

-spec parse_header(known_header(), ersip_sipmsg:sipmsg()) -> ValueOrError when
      ValueOrError :: { ok, term() }
                    | { error, term() }.
parse_header(HdrAtom, Msg) when is_atom(HdrAtom) ->
    Descr = header_descr(HdrAtom),
    case get_header(Descr, Msg) of
        { ok, no_header } ->
            { ok, no_header };
        { ok, Hdr } ->
            parse_header_by_descr(Descr, Hdr);
        { error, _ } = Error ->
            Error
    end.

%%%===================================================================
%%% Internal implementation
%%%===================================================================


-type header_required() :: all        %% Header required for all requests/responses
                         | request    %% Header required in requests
                         | with_body. %% Header required if body is not empty

-record(required_essentials, { type     :: ersip_msg:type(),
                               method   :: ersip_method:method(),
                               status   :: undefined | ersip_status:code(),
                               has_body :: boolean()
                             }).
-type required_essentials() :: #required_essentials{}.

-spec parse_header_by_descr(descr(), ersip_hdr:header()) -> Result when
      Result :: { ok, Value :: term() }
              | { error, term() }.
parse_header_by_descr(#descr{ parse_fun = F }, Hdr) ->
    F(Hdr).

-spec get_header(descr(), ersip_sipmsg:sipmsg()) -> Result when
      Result :: { ok, ersip_hdr:header() }
              | { ok, no_header }
              | { error, { no_required_header, binary() } }.
get_header(#descr{ name = N } = D, SipMsg) ->
    Hdr = ersip_msg:get(N, ersip_sipmsg:raw_message(SipMsg)),
    Required = is_required(SipMsg, D#descr.required),
    case ersip_hdr:is_empty(Hdr) of
        true ->
            case Required of
                true ->
                    { error, { no_required_header, N } };
                false ->
                    { ok, no_header }
            end;
        false ->
            { ok, Hdr }
    end.

-spec is_required(ersip_sipmsg:sipmsg() | required_essentials(), header_required()) -> boolean().
is_required(_, all) ->
    true;
is_required(#required_essentials{ type = T }, T) ->
    true;
is_required(#required_essentials{ has_body = true }, with_body) ->
    true;
is_required(#required_essentials{}, _) ->
    false;
is_required(SipMsg, R) ->
    is_required(required_essentials(SipMsg), R).

-spec required_essentials(ersip_sipmsg:sipmsg()) -> required_essentials().
required_essentials(SipMsg) ->
    Type = ersip_sipmsg:type(SipMsg),
    #required_essentials{
       type     = Type,
       method   = ersip_sipmsg:method(SipMsg),
       status   = ersip_sipmsg:status(SipMsg),
       has_body = ersip_sipmsg:has_body(SipMsg)
      }.


%%%
%%% Headers description
%%%
-spec header_descr(known_header()) -> #descr{}.
header_descr(from) ->
    #descr{ name   = <<"from">>,
            required = all,
            parse_fun = fun ersip_hdr_fromto:parse/1
          };
header_descr(to) ->
    #descr{ name   = <<"to">>,
            required = all,
            parse_fun = fun ersip_hdr_fromto:parse/1
          };
header_descr(cseq) ->
    #descr{ name   = <<"cseq">>,
            required = all,
            parse_fun = fun ersip_hdr_cseq:parse/1
          };
header_descr(callid) ->
    #descr{ name   = <<"call-id">>,
            required = all,
            parse_fun = fun ersip_hdr_callid:parse/1
          };
header_descr(maxforwards) ->
    #descr{ name   = <<"max-forwards">>,
            required = request,
            parse_fun = fun ersip_hdr_maxforwards:parse/1
          };
header_descr(topmost_via) ->
    #descr{ name   = <<"via">>,
            required = all,
            parse_fun = fun ersip_hdr_via:topmost_via/1
          };
header_descr(content_type) ->
    #descr{ name   = <<"content-type">>,
            required = with_body,
            parse_fun = fun ersip_hdr_content_type:parse/1
          }.

