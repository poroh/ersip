%%
%% Copyright (c) 2017, 2018 Dmitry Poroh
%% All rights reserved.
%% Distributed under the terms of the MIT License. See the LICENSE file.
%%
%% Response status codes
%%

-module(ersip_status).

-export([ response_type/1,
          reason_phrase/1
        ]).
-export_type([ response_type/0,
               code/0,
               reason/0 ]).

%%%===================================================================
%%% Types
%%%===================================================================

-type response_type() :: final | provisional.
-type code() :: 100..699.
-type reason() :: binary().

%%%===================================================================
%%% API
%%%===================================================================

%% @doc Convert status code to response type (final or provisional).
-spec response_type(code()) -> response_type().
response_type(StatusCode) when StatusCode >= 200 andalso StatusCode =< 699 ->
    final;
response_type(StatusCode) when StatusCode >= 100 andalso StatusCode =< 199 ->
    provisional.

-spec reason_phrase(code()) -> reason().
reason_phrase(Code) ->
    reason_impl(Code).


%%%===================================================================
%%% Internal Implementation
%%%===================================================================
reason_impl(100) -> <<"Trying">>;
reason_impl(404) -> <<"Not Found">>;
reason_impl(_)   -> <<"Unknown Status">>.
