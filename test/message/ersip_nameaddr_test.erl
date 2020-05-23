%%
%% Copyright (c) 2017 Dmitry Poroh
%% All rights reserved.
%% Distributed under the terms of the MIT License. See the LICENSE file.
%%
%% Name and address test
%%

-module(ersip_nameaddr_test).

-include_lib("eunit/include/eunit.hrl").

nameaddr_parse_test() ->
    Ex1HostBin = <<"bell-telephone.com">>,
    Ex1 = <<"\"A. G. Bell\" <sip:agb@", Ex1HostBin/binary,"> ;tag=a48s">>,
    Ex1URI = ersip_uri:make(<<"sip:agb@", Ex1HostBin/binary>>),
    {ok,
     {{display_name, <<"\"A. G. Bell\"">>},
      Ex1URI
     },
     <<" ;tag=a48s">>
    } = ersip_nameaddr:parse(Ex1),

    Ex2HostBin = <<"biloxi.com">>,
    Ex2 = <<"Bob <sip:bob@biloxi.com>;tag=a6c85cf">>,
    Ex2URI = ersip_uri:make(<<"sip:bob@", Ex2HostBin/binary>>),
    {ok,
     {{display_name, [<<"Bob">>]},
      Ex2URI
     },
     <<";tag=a6c85cf">>
    } = ersip_nameaddr:parse(Ex2),

    Ex3HostBin = <<"biloxi.com">>,
    Ex3 = <<"Bob Smith <sip:bob-smith@biloxi.com>;tag=a6c85cf">>,
    Ex3URI = ersip_uri:make(<<"sip:bob-smith@", Ex3HostBin/binary>>),
    {ok,
     {{display_name, [<<"Bob">>, <<"Smith">>]},
      Ex3URI
     },
     <<";tag=a6c85cf">>
    } = ersip_nameaddr:parse(Ex3),

    Ex4HostBin = <<"biloxi.com">>,
    Ex4 = <<"<sip:bob-smith@biloxi.com>;tag=a6c85cf">>,
    Ex4URI = ersip_uri:make(<<"sip:bob-smith@", Ex4HostBin/binary>>),
    {ok,
     {{display_name, []},
      Ex4URI
     },
     <<";tag=a6c85cf">>
    } = ersip_nameaddr:parse(Ex4),

    Ex5HostBin = <<"biloxi.com">>,
    Ex5 = <<"sip:bob-smith@biloxi.com;tag=a6c85cf">>,
    Ex5URI = ersip_uri:make(<<"sip:bob-smith@", Ex5HostBin/binary>>),
    {ok,
     {{display_name, []},
      Ex5URI
     },
     <<";tag=a6c85cf">>
    } = ersip_nameaddr:parse(Ex5),

    Ex6HostBin = <<"biloxi.com">>,
    Ex6 = <<"sip:bob-smith@biloxi.com">>,
    Ex6URI = ersip_uri:make(<<"sip:bob-smith@", Ex6HostBin/binary>>),
    {ok,
     {{display_name, []},
      Ex6URI
     },
     <<>>
    } = ersip_nameaddr:parse(Ex6).


nameaddr_neg_parse_test() ->
    ?assertMatch({error, {invalid_nameaddr, _}}, ersip_nameaddr:parse(<<>>)),
    %% Not complete UTF-8 in display name
    %% ?assertMatch({error, _}, ersip_nameaddr:parse(<<"\"", 16#c2, "\" <sip:bob-smith@biloxi.com>;tag=a6c85cf">>)),
    %% No LAQUOT
    ?assertMatch({error, {invalid_nameaddr, _}}, ersip_nameaddr:parse(<<"sip:bob-smith@biloxi.com>">>)),
    %% No RAQUOT
    ?assertMatch({error, {invalid_nameaddr, _}}, ersip_nameaddr:parse(<<"<sip:bob-smith@biloxi.com">>)),
    %% Ivalid host name
    ?assertMatch({error, {invalid_nameaddr, _}}, ersip_nameaddr:parse(<<"<sip:bob-smith@biloxi.->">>)),
    ?assertMatch({error, {invalid_nameaddr, _}}, ersip_nameaddr:parse(<<"1.2.3.4">>)),
    ?assertMatch({error, {invalid_nameaddr, _}}, ersip_nameaddr:parse(<<"?:1.2.3.4">>)),
    ?assertMatch({error, {invalid_nameaddr, _}}, ersip_nameaddr:parse(<<"1.2.3.4;tag=a6c85cf">>)),
    ?assertMatch({error, {invalid_nameaddr, _}}, ersip_nameaddr:parse(<<"\"A B C\"">>)),
    ?assertMatch({error, {invalid_nameaddr, _}}, ersip_nameaddr:parse(<<"\"A B C\"Abc <sip:a@b>">>)),
    ok.

nameaddr_assemble_display_name_test() ->
    {ok, {DNBob,  _}, _} = ersip_nameaddr:parse(<<"Bob Smith <sip:bob-smith@biloxi.com>;tag=a6c85cf">>),
    {ok, {DNBell, _}, _} = ersip_nameaddr:parse(<<"\"A. G. Bell\" <sip:agb@bell-telephone.com> ;tag=a48s">>),
    ?assertEqual(<<"Bob Smith">>, ersip_nameaddr:assemble_display_name_bin(DNBob)),
    ?assertEqual(<<"\"A. G. Bell\"">>, ersip_nameaddr:assemble_display_name_bin(DNBell)),
    ok.


assemble_display_name_test() ->
    [ ?assertEqual(ersip_nameaddr:assemble_display_name(DN), ersip_display_name:assemble(DN))
      || DN <- [ersip_display_name:make(<<"Alice">>),
                ersip_display_name:make(<<"\"Bob\"">>),
                ersip_display_name:make(<<"Theodore \"Teddy\" Roosevelt">>)]],
    [ ?assertEqual(ersip_nameaddr:assemble_display_name_bin(DN), ersip_display_name:assemble_bin(DN))
      || DN <- [ersip_display_name:make(<<"Alice">>),
                ersip_display_name:make(<<"\"Bob\"">>),
                ersip_display_name:make(<<"Theodore \"Teddy\" Roosevelt">>)]],
    ok.
