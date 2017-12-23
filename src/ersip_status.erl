%%
%% Copyright (c) 2017 Dmitry Poroh
%% All rights reserved.
%% Distributed under the terms of the MIT License. See the LICENSE file.
%%
%% Response status codes
%%

-module(ersip_status).

-export([ response_type/1 ]).

-type response_type() :: final | provisional.
-type code() :: 100..699.
-export_type([ response_type/0,
               code/0 ]).

%%%===================================================================
%%% API
%%%===================================================================

%% @doc Convert status code to response type (final or provisional).
-spec response_type(code()) -> response_type().
response_type(StatusCode) when StatusCode >= 200 andalso StatusCode =< 699 ->
    final;
response_type(StatusCode) when StatusCode >= 100 andalso StatusCode =< 199 ->
    provisional.
