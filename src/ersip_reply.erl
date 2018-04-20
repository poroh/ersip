%%
%% Copyright (c) 2018 Dmitry Poroh
%% All rights reserved.
%% Distributed under the terms of the MIT License. See the LICENSE file.
%%
%% Defines ways of replying on the SIP request
%%

-module(ersip_reply).

-export([new/1,
         new/2,
         status/1,
         reason/1,
         to_tag/1
        ]).
-export_type([options/0]).

%%%===================================================================
%%% Types
%%%===================================================================

-record(options, {status             :: ersip_status:code(),
                  reason = undefined :: undefined | ersip_status:reason(),
                  to_tag = undefined :: undefined | ersip_hdr_fromto:tag()
                 }).
-type options()    :: #options{}.
-type param_pair() :: {know_param(), term()}.
-type know_param() :: reason
                    | to_tag.

%%%===================================================================
%%% API
%%%===================================================================

-spec new(ersip_status:code()) -> options().
new(Status) when is_integer(Status) 
                 andalso Status >= 100
                 andalso Status =< 699 ->
    #options{status = Status}.

-spec new(ersip_status:code(), Params) -> options() when
      Params :: [param_pair()].         
new(Status, Params) when is_integer(Status) 
                         andalso Status >= 100
                         andalso Status =< 699 ->
    Opts0 = #options{status = Status},
    lists:foldl(fun add_param/2,
                Opts0,
                Params).

-spec status(#options{}) -> ersip_status:code().
status(#options{status = Status}) -> 
    Status.

-spec reason(#options{}) -> binary().
reason(#options{reason = undefined, status = Status}) ->
    ersip_status:reason_phrase(Status);
reason(#options{reason = Reason}) ->
    Reason.

-spec to_tag(options()) -> undefined | ersip_hdr_fromto:tag().
to_tag(#options{to_tag = Tag}) ->
    Tag.

%%%===================================================================
%%% Internal Implementation
%%%===================================================================

add_param({to_tag, Tag}, #options{} = Opts) ->
    Opts#options{to_tag = Tag};
add_param({reason, Reason}, #options{} = Opts) ->
    Opts#options{reason = Reason}.

