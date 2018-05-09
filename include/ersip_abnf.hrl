%%
%% Copyright (c) 2017 Dmitry Poroh
%% All rights reserved.
%% Distributed under the terms of the MIT License. See the LICENSE file.
%%
%% RFC2234 ABNF for Syntax Specifications
%% Useful syntax defines
%%

%% ALPHA =  %x41-5A / %x61-7A   ; A-Z / a-z
-define(is_ALPHA(X), ((X >= $A andalso X =< $Z) orelse (X >= $a andalso X =< $z))).

%% DIGIT          =  %x30-39    ; 0-9
-define(is_DIGIT(X), (X >= $0 andalso X =< $9)).

%% HEXDIG         =  DIGIT / "A" / "B" / "C" / "D" / "E" / "F"
-define(is_HEXDIG(X), (?is_DIGIT(X) orelse (X >= $A andalso X =< $F))).
