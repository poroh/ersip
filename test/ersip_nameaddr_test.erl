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
    Ex1 = <<"\"A. G. Bell\" <sip:agb@bell-telephone.com> ;tag=a48s">>,
    Ex1Host = ersip_host:make(<<"bell-telephone.com">>),
    Ex1URI = ersip_uri:make([{user, <<"agb">>}, {host, Ex1Host}]),
    {ok,
     {{display_name, <<"\"A. G. Bell\"">>},
      Ex1URI
     },
     <<" ;tag=a48s">>
    } = ersip_nameaddr:parse(Ex1),

    Ex2 = <<"Bob <sip:bob@biloxi.com>;tag=a6c85cf">>,
    Ex2Host = ersip_host:make(<<"biloxi.com">>),
    Ex2URI = ersip_uri:make([{user, <<"bob">>}, {host, Ex2Host}]),
    {ok,
     {{display_name, [<<"Bob">>]},
      Ex2URI
     },
     <<";tag=a6c85cf">>
    } = ersip_nameaddr:parse(Ex2),

    Ex3 = <<"Bob Smith <sip:bob-smith@biloxi.com>;tag=a6c85cf">>,
    Ex3Host = ersip_host:make(<<"biloxi.com">>),
    Ex3URI = ersip_uri:make([{user, <<"bob-smith">>}, {host, Ex3Host}]),
    {ok,
     {{display_name, [<<"Bob">>, <<"Smith">>]},
      Ex3URI
     },
     <<";tag=a6c85cf">>
    } = ersip_nameaddr:parse(Ex3),

    Ex4 = <<"<sip:bob-smith@biloxi.com>;tag=a6c85cf">>,
    Ex4Host = ersip_host:make(<<"biloxi.com">>),
    Ex4URI = ersip_uri:make([{user, <<"bob-smith">>}, {host, Ex4Host}]),
    {ok,
     {{display_name, []},
      Ex4URI
     },
     <<";tag=a6c85cf">>
    } = ersip_nameaddr:parse(Ex4),

    Ex5 = <<"sip:bob-smith@biloxi.com;tag=a6c85cf">>,
    Ex5Host = ersip_host:make(<<"biloxi.com">>),
    Ex5URI = ersip_uri:make([{user, <<"bob-smith">>}, {host, Ex5Host}]),
    {ok,
     {{display_name, []},
      Ex5URI
     },
     <<";tag=a6c85cf">>
    } = ersip_nameaddr:parse(Ex5),

    Ex6 = <<"sip:bob-smith@biloxi.com">>,
    Ex6Host = ersip_host:make(<<"biloxi.com">>),
    Ex6URI = ersip_uri:make([{user, <<"bob-smith">>}, {host, Ex6Host}]),
    {ok,
     {{display_name, []},
      Ex6URI
     },
     <<>>
    } = ersip_nameaddr:parse(Ex6).


nameaddr_neg_parse_test() ->
    ?assertMatch({error, _}, ersip_nameaddr:parse(<<>>)),
    %% Not complete UTF-8 in display name
    ?assertMatch({error, _}, ersip_nameaddr:parse(<<"\"", 16#c2, "\" <sip:bob-smith@biloxi.com>;tag=a6c85cf">>)),
    %% No LAQUOT
    ?assertMatch({error, _}, ersip_nameaddr:parse(<<"sip:bob-smith@biloxi.com>">>)),
    %% No RAQUOT
    ?assertMatch({error, _}, ersip_nameaddr:parse(<<"<sip:bob-smith@biloxi.com">>)),
    %% Ivalid host name
    ?assertMatch({error, _}, ersip_nameaddr:parse(<<"<sip:bob-smith@biloxi.->">>)),
    ?assertMatch({error, _}, ersip_nameaddr:parse(<<"1.2.3.4">>)),
    ?assertMatch({error, _}, ersip_nameaddr:parse(<<"?:1.2.3.4">>)),
    ?assertMatch({error, _}, ersip_nameaddr:parse(<<"1.2.3.4;tag=a6c85cf">>)).
