%%
%% Copyright (c) 2018 Dmitry Poroh
%% All rights reserved.
%% Distributed under the terms of the MIT License. See the LICENSE file.
%%
%% SIP Contact headers
%%

-module(ersip_hdr_contact_list).

-export([make/1,
         make_star/0,
         build/2,
         parse/1
        ]).

-export_type([contact_list/0]).

%%%===================================================================
%%% Types
%%%===================================================================

-type star_contact() :: star.
-type contact_list() :: star_contact()
                      | [ersip_hdr_contact:contact()].

-type parse_result() :: {ok, contact_list()}
                      | {error, term()}.

-type maybe_rev_contact_list() :: {ok, contact_list()}
                                | {error, term()}.

%%%===================================================================
%%% API
%%%===================================================================

-spec make(iolist() | binary()) -> contact_list().
make(Binary) ->
    H0 = ersip_hdr:new(<<"Contact">>),
    H1 = ersip_hdr:add_value(Binary, H0),
    case parse(H1) of
        {ok, ContactList} ->
            ContactList;
        {error, Reason} ->
            error(Reason)
    end.

-spec make_star() -> star_contact().
make_star() ->
    star.

-spec build(HeaderName :: binary(), contact_list()) -> ersip_hdr:header().
build(HdrName, star) ->
    Hdr = ersip_hdr:new(HdrName),
    ersip_hdr:add_value(<<"*">>, Hdr);
build(HdrName, ContactList) when is_list(ContactList) ->
    Hdr = ersip_hdr:new(HdrName),
    lists:foldl(
      fun(Contact, HdrAcc) ->
              ersip_hdr:add_value(ersip_hdr_contact:assemble(Contact), HdrAcc)
      end,
      Hdr,
      ContactList).


%% Contact        =  ("Contact" / "m" ) HCOLON
%%                   ( STAR / (contact-param *(COMMA contact-param)))
-spec parse(ersip_hdr:header()) -> parse_result().
parse(Header) ->
    MaybeRevContactList =
        lists:foldl(fun(IOContact, Acc) ->
                            add_to_maybe_contact_list(iolist_to_binary(IOContact), Acc)
                    end,
                    {ok, []},
                    ersip_hdr:raw_values(Header)),
    case MaybeRevContactList of
        {ok, star} ->
            {ok, star};
        {ok, RevContactList} ->
            {ok, lists:reverse(RevContactList)};
        Error ->
            Error
    end.

%%%===================================================================
%%% Internal implementation
%%%===================================================================

-spec add_to_maybe_contact_list(binary(), maybe_rev_contact_list()) -> maybe_rev_contact_list().
add_to_maybe_contact_list(_, {error, _} = Error) ->
    Error;
add_to_maybe_contact_list(<<"*">>, {ok, []}) ->
    {ok, star};
add_to_maybe_contact_list(_, {ok, star}) ->
    {error, {invalid_contact, <<"multiple contacts and star are invalid">>}};
add_to_maybe_contact_list(<<>>, {ok, _} = Result) ->
    Result;
add_to_maybe_contact_list(Bin, {ok, ContactList}) when is_list(ContactList) ->
    case ersip_hdr_contact:parse_hdr(Bin) of
        {ok, Contact, Rest0} ->
            case ersip_bin:trim_head_lws(Rest0) of
                <<>> ->
                    {ok, [Contact | ContactList]};
                <<",", Rest1/binary>> ->
                    Rest2 = ersip_bin:trim_head_lws(Rest1),
                    add_to_maybe_contact_list(Rest2, {ok, [Contact | ContactList]})
            end;
        {error, _} = Error ->
            io:format("add_to_maybe_contact_list: ~p", [Bin]),
            Error
    end.
