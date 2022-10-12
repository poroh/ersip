%%%
%%% Copyright (c) 2019, 2020 Dmitry Poroh
%%% All rights reserved.
%%% Distributed under the terms of the MIT License. See the LICENSE file.
%%%
%%% SIP Refer-To header
%%%
%%% RFC 3515
%%%

-module(ersip_hdr_refer_to).

-export([new/1,
         uri/1,
         set_uri/2,
         display_name/1,
         set_display_name/2,
         all_raw_params/1,
         parse/1,
         make/1,
         assemble/1,
         assemble_bin/1,
         build/2,
         raw/1
        ]).

-export_type([refer_to/0, raw/0]).

%%===================================================================
%% Types
%%===================================================================

-record(refer_to, {display_name :: ersip_display_name:display_name(),
                   uri          :: ersip_uri:uri(),
                   hparams      :: ersip_hparams:hparams()}).
-type refer_to() :: #refer_to{}.
-type parse_result() :: {ok, refer_to()} | {error, term()}.

-type uri()      :: ersip_uri:raw().
-type uri_bin()  :: ersip_uri:raw() | binary().

-type raw(T)     :: #{uri          := T,
                      params       := ersip_hparams:raw(),
                      display_name := ersip_display_name:raw()}.


-type raw()      :: raw(uri()).
-type raw_make() :: raw(uri_bin()).


%%===================================================================
%% API
%%===================================================================

%% @doc Create Refer-To header from URI.
-spec new(ersip_uri:uri()) -> refer_to().
new(URI) ->
    #refer_to{display_name = {display_name, []},
              uri = URI,
              hparams = ersip_hparams:new()}.

%% @doc URI in Refer-To header.
-spec uri(refer_to()) -> ersip_uri:uri().
uri(#refer_to{uri = URI}) ->
    URI.

%% @doc Set URI of Refer-To header.
-spec set_uri(ersip_uri:uri(), refer_to()) -> refer_to().
set_uri(URI, #refer_to{} = RT) ->
    RT#refer_to{uri = URI}.

%% @doc Display name in Refer-To header.
-spec display_name(refer_to()) -> ersip_nameaddr:display_name().
display_name(#refer_to{display_name = DN}) ->
    DN.

%% @doc Set display name of Refer-To header.
-spec set_display_name(ersip_nameaddr:display_name(), refer_to()) -> refer_to().
set_display_name({display_name, _} = DN, #refer_to{} = RT) ->
    RT#refer_to{display_name = DN}.

%% @doc Get all raw parmeters of Refer-To header.
-spec all_raw_params(refer_to()) -> [{binary(), binary()} | binary()].
all_raw_params(#refer_to{hparams = HParams}) ->
    ersip_hparams:to_raw_list(HParams).

%% @doc Parse Refer-To from binary or raw SIP header representation.
-spec parse(ersip_hdr:header() | binary()) -> parse_result().
parse(Bin) when is_binary(Bin) ->
    parse_hdr(Bin);
parse(Header) ->
    case ersip_hdr:raw_values(Header) of
        [] ->
            {error, no_refer_to};
        [ReferToIOList]  ->
            parse_hdr(iolist_to_binary(ReferToIOList))
    end.

%% @doc Make Refer-To from binary or raw value.
-spec make(binary() | raw_make()) -> refer_to().
make(Bin) when is_binary(Bin) ->
    case parse_hdr(Bin) of
        {ok, ReferTo} ->
            ReferTo;
        {error, Reason} ->
            error(Reason)
    end;
make(#{uri := URI} = Raw) ->
    HParams0 = ersip_hparams:make(maps:get(params, Raw, #{})),
    HParams =
        case ersip_hparams:parse_known(fun parse_known/2, HParams0) of
            {ok, H} -> H
            %% {error, Reason} -> error({invalid_params, Reason}) %% (Ref1) see below
        end,
    #refer_to{display_name  = ersip_display_name:make(maps:get(display_name, Raw, <<>>)),
              uri           = ersip_uri:make(URI),
              hparams       = HParams
             }.

%% @doc Assemble Refer-To to iolist()
-spec assemble(refer_to()) -> iolist().
assemble(#refer_to{} = ReferTo) ->
    #refer_to{display_name = DisplayName,
              uri = URI,
              hparams = HParams
             } = ReferTo,
    HParamsIO0 = ersip_hparams:assemble(HParams),
    HParamsIO =
        case ersip_iolist:is_empty(HParamsIO0) of
            true -> [];
            false -> [$; | HParamsIO0]
        end,
    [ersip_nameaddr:assemble(DisplayName, URI), HParamsIO].

%% @doc Assemble Refer-To to binary().
-spec assemble_bin(refer_to()) -> binary().
assemble_bin(#refer_to{} = ReferTo) ->
    iolist_to_binary(assemble(ReferTo)).

%% @doc Build SIP raw header.
-spec build(HdrName :: binary(), refer_to()) -> ersip_hdr:header().
build(HdrName, #refer_to{} = ReferTo) ->
    Hdr = ersip_hdr:new(HdrName),
    ersip_hdr:add_value(assemble(ReferTo), Hdr).

%% @doc Raw representation of Refer-To header.
-spec raw(refer_to()) -> raw().
raw(#refer_to{} = ReferTo) ->
    #{uri => ersip_uri:raw(uri(ReferTo)),
      display_name => ersip_display_name:raw(display_name(ReferTo)),
      params => ersip_hparams:raw(ReferTo#refer_to.hparams)
     }.

%%===================================================================
%% Internal implementation
%%===================================================================

-spec parse_hdr(binary()) -> parse_result().
parse_hdr(Bin) ->
    %% Intentionally not fully compliant parser - prefer to add
    %% ;param=value to the URI instead of to headers parameters that
    %% are not defined by RFC. A lot of bad implementations uses something like this:
    %% Refer-To: sip:111@1.1.1.1;transport=tcp?Replaces=calllid;to-tag=a;from-tag=b
    Parsers = [fun(NameAddr) -> ersip_nameaddr:parse(NameAddr, [<<" ">>, <<"\t">>]) end,
               fun ersip_parser_aux:trim_lws/1,
               fun parse_params/1
              ],
    case ersip_parser_aux:parse_all(Bin, Parsers) of
        {ok, [{DisplayName, URI}, _, HParams], <<>>} ->
            ReferTo = #refer_to{display_name = DisplayName,
                                uri          = URI,
                                hparams      = HParams},
            {ok, ReferTo};
        {ok, _, Rest} ->
            {error, {invalid_refer_to, {garbage_at_end, Rest}}};
        {error, Reason} ->
            {error, {invalid_refer_to, Reason}}
    end.

%% @private
-spec parse_params(binary()) -> ersip_parser_aux:parse_result(ersip_parser_aux:gen_param_list()).
parse_params(<<$;, Bin/binary>>) ->
    ersip_hparams:parse(fun parse_known/2, Bin);
parse_params(Bin) ->
    {ok, ersip_hparams:new(), Bin}.

%% @private
%%
%% WARNING: If you add known parameter here then you need to add handling
%% parse known result above (see Ref1).
-spec parse_known(binary(), binary()) -> ersip_hparams:parse_known_fun_result().
parse_known(_, _) ->
    {ok, unknown}.
