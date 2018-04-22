%%
%% Copyright (c) 2017 Dmitry Poroh
%% All rights reserved.
%% Distributed under the terms of the MIT License. See the LICENSE file.
%%
%% Common SIP transaction interface
%%

-module(ersip_trans).
-export([id/1]).

-type trans()  :: ersip_uac_fsm:uac()
                | ersip_uas_fsm:uas().
-type tid()    :: {tid, ersip_trans_id:transaction_id()}.

-export_type([trans/0,
              tid/0
             ]).

%%%===================================================================
%%% API
%%%===================================================================

-spec id(trans()) -> tid().
id(Trans) ->
    call_trans_module(id, Trans, [Trans]).

%%%===================================================================
%%% Internal implementation
%%%===================================================================

call_trans_module(FunId, Transaction, Args) ->
    Module =
        case classify(Transaction) of
            uac -> ersip_uac_fsm;
            uas -> ersip_uas_fsm
        end,
    erlang:apply(Module, FunId, Args).

-spec classify(trans()) -> uac | uas.
classify(Trans) ->
    element(1, Trans).
