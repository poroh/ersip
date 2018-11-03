%%
%% Copyright (c) 2018 Dmitry Poroh
%% All rights reserved.
%% Distributed under the terms of the MIT License. See the LICENSE file.
%%
%% SIP Call-ID header
%%

-module(ersip_hdr_callid).
-include("ersip_sip_abnf.hrl").

-export([make/1,
         make_key/1,
         parse/1,
         build/2,
         assemble/1
        ]).

-export_type([callid/0]).

%%%===================================================================
%%% Types
%%%===================================================================

-type callid() :: {callid, binary()}.

%%%===================================================================
%%% API
%%%===================================================================

-spec make(ersip_hdr:header() | binary()) -> callid().
make(Bin) when is_binary(Bin) ->
    case parse_callid(Bin) of
        {ok, CallId} ->
            CallId;
        Error ->
            error(Error)
    end;
make(Header) ->
    case parse(Header) of
        {ok, CallId} ->
            CallId;
        Error ->
            error(Error)
    end.

-spec make_key(callid()) -> callid().
make_key({callid, _} = C) ->
    C.

-spec parse(ersip_hdr:header() | binary()) -> Result when
      Result :: {ok, callid()}
              | {error, Error},
      Error :: no_callid
             | {invalid_callid, binary()}.
parse(Value)  when is_binary(Value)->
    parse_callid(Value);
parse(Header) ->
    case ersip_hdr:raw_values(Header) of
        [] ->
            {error, no_callid};
        [CallIdIOList]  ->
            parse_callid(iolist_to_binary(CallIdIOList))
    end.

-spec build(HeaderName :: binary(), callid()) -> ersip_hdr:header().
build(HdrName, {callid, _} = CallId) ->
    Hdr = ersip_hdr:new(HdrName),
    ersip_hdr:add_value([assemble(CallId)], Hdr).

-spec assemble(callid()) ->  binary().
assemble({callid, CallIdBin}) ->
    CallIdBin.

%%%===================================================================
%%% Internal implementation
%%%===================================================================

%% callid   =  word ["@" word]
-spec parse_callid(binary()) -> Result when
      Result :: {ok, callid()}
              | {error, Error},
      Error  :: {invalid_callid, binary()}.
parse_callid(Binary) ->
    Words = binary:split(Binary, <<"@">>),
    case lists:all(fun is_word/1, Words) of
        true ->
            {ok, {callid, Binary}};
        false ->
            {error, {invalid_callid, Binary}}
    end.

%% word     =  1*(alphanum / "-" / "." / "!" / "%" / "*" /
%%             "_" / "+" / "`" / "'" / "~" /
%%             "(" / ")" / "<" / ">" /
%%             ":" / "\" / DQUOTE /
%%             "/" / "[" / "]" / "?" /
%%             "{" / "}" )
-spec is_word(binary()) -> boolean().
is_word(<<>>) ->
    false;
is_word(<<C/utf8>>) ->
    is_wordc(C);
is_word(<<C/utf8, Rest/binary>>) ->
    case is_wordc(C) of
        false ->
            false;
        true ->
            is_word(Rest)
    end.

-spec is_wordc(char()) -> boolean().
is_wordc(C) when ?is_alphanum(C) -> true;

is_wordc($-) -> true;
is_wordc($.) -> true;
is_wordc($!) -> true;
is_wordc($%) -> true;
is_wordc($*) -> true;

is_wordc($_) -> true;
is_wordc($+) -> true;
is_wordc($`) -> true;
is_wordc($') -> true;
is_wordc($~) -> true;

is_wordc($() -> true;
is_wordc($)) -> true;
is_wordc($<) -> true;
is_wordc($>) -> true;

is_wordc($:) -> true;
is_wordc($\\) -> true;
is_wordc($") -> true;

is_wordc($/) -> true;
is_wordc($[) -> true;
is_wordc($]) -> true;
is_wordc($?) -> true;
is_wordc(${) -> true;
is_wordc($}) -> true;

is_wordc(_) -> false.
