%%
%% Copyright (c) 2018 Dmitry Poroh
%% All rights reserved.
%% Distributed under the terms of the MIT License. See the LICENSE file.
%%
%% Side effects of user agents
%%

-module(ersip_ua_se).

-export([ua_result/1,
         send_response/1
        ]).

ua_result(SipMsg) ->
    {ua_result, SipMsg}.

send_response(SipMsg) ->
    {send_response, SipMsg}.
