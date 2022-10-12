%%%
%%% Copyright (c) 2017 Dmitry Poroh
%%% All rights reserved.
%%% Distributed under the terms of the MIT License. See the LICENSE file.
%%%
%%% IO list routines
%%%

-module(ersip_iolist).

-export([trim_head_lws/1,
         trim_tail_lws/1,
         is_empty/1,
         join/2
        ]).

-include("ersip_sip_abnf.hrl").

%%===================================================================
%% API
%%===================================================================

%% @doc trim leading linear whitesapaces (SP or HTABs).
-spec trim_head_lws(iolist()|binary()) -> iolist()|binary().
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

%% @doc trim trailing linear whitesapaces (SP or HTABs).
-spec trim_tail_lws(iolist()|binary()) -> iolist()|binary().
trim_tail_lws(L) ->
    trim_tail_lws_impl(L).

-spec is_empty(iolist()|binary()) -> boolean().
is_empty(<<>>) ->
    true;
is_empty([]) ->
    true;
is_empty(X) when not is_list(X) ->
    false;
is_empty([X|Rest]) ->
    case is_empty(X) of
        false ->
            false;
        true ->
            is_empty(Rest)
    end.


%% @doc join list of iolists with separator
-spec join(Sep :: iolist() | binary() | char(), List :: [iolist() | binary()]) -> iolist().
join(_, [])  -> [];
join(_, [X]) -> X;
join(Sep, [X | Rest]) ->
    [X | lists:map(fun(Y) -> [Sep, Y] end, Rest)].

%%%===================================================================
%%% internal implementation
%%%===================================================================

-ifdef(OTP_RELEASE).

string_trim_lws(Str) ->
    string_trim_lws(?OTP_RELEASE, Str).

string_trim_lws(Release, Str) when Release >= 20 ->
    string:trim(Str, leading, [ $ , $\t ]);
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

-spec trim_tail_lws_impl(iolist()|binary()) -> iolist()|binary().
trim_tail_lws_impl(X) when is_binary(X) ->
    ersip_bin:trim_tail_lws(X);
trim_tail_lws_impl(Char) when ?is_LWS_char(Char) ->
    [];
trim_tail_lws_impl(X) when not is_list(X) ->
    X;
trim_tail_lws_impl([]) ->
    [];
trim_tail_lws_impl([ X | Rest ]) ->
    IoList = trim_tail_lws_impl(Rest),
    case is_empty(IoList) of
        true ->
            XTrimmed = trim_tail_lws_impl(X),
            case is_empty(XTrimmed) of
                true ->
                    [];
                false ->
                    [ XTrimmed ]
            end;
        false ->
            [ X | IoList ]
    end.
