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


%% mark        =  "-" / "_" / "." / "!" / "~" / "*" / "'"
%%                 / "(" / ")"
-define(is_mark(X),
        (X =:= $- 
         orelse X =:= $_ 
         orelse X =:= $.
         orelse X =:= $!
         orelse X =:= $~
         orelse X =:= $*
         orelse X =:= $'
         orelse X =:= $(
         orelse X =:= $))).

%% unreserved  =  alphanum / mark
-define(is_unreserved(X), (?is_alphanum(X) orelse ?is_mark(X))).    

%% user-unreserved  =  "&" / "=" / "+" / "$" / "," / ";" / "?" / "/"
-define(is_user_unreserved(X),
        (X  =:= $&
         orelse X =:= $=
         orelse X =:= $+
         orelse X =:= $$
         orelse X =:= $,
         orelse X =:= $;
         orelse X =:= $?
         orelse X =:= $/
        )).

%% token       =  1*(alphanum / "-" / "." / "!" / "%" / "*"
%%                   / "_" / "+" / "`" / "'" / "~" )
-define(is_token_char(X), (?is_alphanum(X)
                           orelse X =:= $-
                           orelse X =:= $.
                           orelse X =:= $!
                           orelse X =:= $%
                           orelse X =:= $*
                           orelse X =:= $_
                           orelse X =:= $+
                           orelse X =:= $`
                           orelse X =:= $'
                           orelse X =:= $~
                          )).

-define(is_LWS_char(X), (X =:= 32 orelse X =:= $\t)). 
