%%
%% Copyright (c) 2017 Dmitry Poroh
%% All rights reserved.
%% Distributed under the terms of the MIT License. See the LICENSE file.
%%
%% Common SIP transaction interface
%%

-module(ersip_trans).
-export([ id/1 ]).

-type trans() :: ersip_uac:uac()
               | ersip_uas:uas().
-type tid()   :: { tid, term() }. 

-export_type([ trans/0, tid/0 ]).

%%%===================================================================
%%% API
%%%===================================================================

-spec id(trans()) -> tid().
id(Trans) ->
    call_trans_module(id, Trans, [ Trans ]).

%%%===================================================================
%%% Internal implementation
%%%===================================================================

call_trans_module(FunId, Transaction, Args) ->
    Module = 
        case classify(Transaction) of
            uac -> ersip_uac;
            uas -> ersip_uas
        end,
    erlang:apply(Module, FunId, Args).

-spec classify(trans()) -> uac | uas.
classify(Trans) ->
    element(1, Trans).
