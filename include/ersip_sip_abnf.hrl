%%
%% Copyright (c) 2017 Dmitry Poroh
%% All rights reserved.
%% Distributed under the terms of the MIT License. See the LICENSE file.
%%
%% RFC3261 SIP ABNF  
%% Useful syntax defines
%%

-include("ersip_abnf.hrl").

%%alphanum  =  ALPHA / DIGIT
-define(is_alphanum(X), (?is_ALPHA(X) orelse ?is_DIGIT(X))).
