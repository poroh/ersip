%%
%% Copyright (c) 2017, 2018 Dmitry Poroh
%% All rights reserved.
%% Distributed under the terms of the MIT License. See the LICENSE file.
%%
%% Common SIP transaction interface
%%

-module(ersip_trans).
-export([new_server/2,
         new_client/3,
         id/1]).

-export_type([trans/0,
              tid/0
             ]).

%%%===================================================================
%%% Types
%%%===================================================================

-record(trans,
        {id        :: ersip_trans:tid(),
         module   :: ersip_trans_client | ersip_trans_server,
         instance :: trans_instance()
        }).
-type trans() :: #trans{}.
-type trans_instance()  :: ersip_trans_client:trans_client()
                         | ersip_trans_server:trans_server().
-type tid() :: {tid, ersip_trans_id:transaction_id()}.
-type result() :: {trans(), ersip_trans_se:effect()}.

%%%===================================================================
%%% API
%%%===================================================================

%%% @doc Create server transaction by message.
-spec new_server(ersip_sipmsg:sipmsg(), ersip:sip_options()) -> result().
new_server(SipMsg, Options) ->
    Id = ersip_trans_id:make_server(SipMsg),
    {Instance, SE} = ersip_trans_server:new(transport_type_by_source(SipMsg), SipMsg, Options),
    Trans = #trans{id       = Id,
                   module   = ersip_trans_server,
                   instance = Instance
                  },
    {Trans, SE}.

-spec new_client(ersip_request:request(), ersip_transport:transport(), ersip:sip_options()) -> result().
new_client(OutReq, Transport, Options) ->
    Id = ersip_request:branch(OutReq),
    TransportType = transport_type_by_transport(Transport),
    {Instance, SE} = ersip_trans_client:new(TransportType, OutReq, Options),
    Trans = #trans{id = Id,
                   module = ersip_trans_client,
                   instance = Instance
                  },
    {Trans, wrap_se_list(SE, Trans)}.

-spec id(trans()) -> tid().
id(#trans{id = Id}) ->
    Id.

%%%===================================================================
%%% Internal implementation
%%%===================================================================

call_trans_module(FunId, #trans{module = Module}, Args) ->
    erlang:apply(Module, FunId, Args).

-spec transport_type_by_source(ersip_sipmsg:sipmsg()) -> reliable | unreliable.
transport_type_by_source(SipMsg) ->
    MsgSource = ersip_sipmsg:source(SipMsg),
    MsgTransport = ersip_source:transport(MsgSource),
    transport_type_by_transport(MsgTransport).

-spec transport_type_by_transport(ersip_transport:transport()) -> reliable | unreliable.
transport_type_by_transport(Transport) ->
    case ersip_transport:is_reliable(Transport) of
        true ->
            reliable;
        false ->
            unreliable
    end.


-spec wrap_se_list([ersip_trans_se:effect()], trans()) -> ersip_trans_se:effect().
wrap_se_list(SideEffects, Trans) ->
    lists:map(fun(SE) -> wrap_se(SE, Trans) end, SideEffects).

-spec wrap_se(ersip_trans_se:effect(), trans()) -> ersip_trans_se:effect().
wrap_se({clear_trans, _}, #trans{id = Id}) ->
    {clear_trans, Id};
wrap_se(SE, _Trans) ->
    SE.

