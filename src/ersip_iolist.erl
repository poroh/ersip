%%
%% Copyright (c) 2017 Dmitry Poroh
%% All rights reserved.
%% Distributed under the terms of the MIT License. See the LICENSE file.
%%
%% IO list routines
%%

-module(ersip_iolist).

-export([ trim_head_lws/1 ]).

%%%===================================================================
%%% API
%%%===================================================================

%% @doc trim leading linear whitesapaces (SP or HTABs).
-spec trim_head_lws(iolist()) -> iolist().
trim_head_lws([]) ->
    [];
trim_head_lws([H|Rest]) when is_binary(H) ->
    case ersip_bin:trim_head_lws(H) of
        <<>> ->
            trim_head_lws(Rest);
        B ->
            [ B | Rest ]
    end;
trim_head_lws([H|Rest]) when is_list(H) ->
    case string_trim_lws(H) of
        [] ->
            trim_head_lws(Rest);
        Str ->
            [ Str | Rest ]
    end;
trim_head_lws(L) when is_list(L) ->
    string_trim_lws(L);
trim_head_lws(B) when is_binary(B) ->
    ersip_bin:trim_head_lws(B).

%%%===================================================================
%%% internal implementation
%%%===================================================================

-ifdef(OTP_RELEASE).

string_trim_lws(Str) ->
    string_trim(?OTP_RELEASE, Str).
string_trim_lws(Release, Str) when Release >= 20 ->
    string:trim(L, leading, [ $ , $\t ]);
string_trim_lws(_, Str) ->
    string_trim_lws_impl(Str).

-else.

string_trim_lws(Str) ->
    string_trim_lws_impl(Str).

-endif.

string_trim_lws_impl([]) ->
    [];
string_trim_lws_impl([ $  | Rest ]) ->
    string_trim_lws_impl(Rest);
string_trim_lws_impl([ $\t | Rest ]) ->
    string_trim_lws_impl(Rest);
string_trim_lws_impl(V) when is_list(V) ->
    V.
