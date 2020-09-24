%%
%% Copyright (c) 2020 Dmitry Poroh
%% All rights reserved.
%% Distributed under the terms of the MIT License. See the LICENSE file.
%%
%% SDP media ice layers // ice candidate
%%

-module(ersip_sdp_ice_candidate).

-export([foundation/1,
         component_id/1,
         transport/1,
         priority/1,
         connection_address/1,
         set_connection_address/2,
         port/1,
         cand_type/1,
         rel_addr/1,
         set_rel_addr/2,
         rel_port/1,
         attrs/1,
         parse/1,
         assemble/1,
         assemble_bin/1
        ]).

-export_type([ice_candidate/0]).

%%%===================================================================
%%% Types
%%%===================================================================

-record(ice_candidate, {foundation          :: ice_candidate_foundation(),
                        component_id        :: ice_candidate_component_id(),
                        transport           :: binary(),
                        priority            :: pos_integer(),
                        connection_address  :: ersip_sdp_addr:addr(),
                        port                :: inet:port_number(),
                        cand_type           :: ice_candidate_cand_type(),
                        rel_addr            :: ersip_sdp_addr:addr() | undefined,
                        rel_port            :: inet:port_number() | undefined,
                        attrs = []          :: ice_candidate_extension_attr_list()
                       }).

-type ice_candidate() :: #ice_candidate{}.

-type ice_candidate_foundation()   :: binary().
-type ice_candidate_component_id() :: 1..256.
-type ice_candidate_cand_type()    :: binary().

-type ice_candidate_extension_attr()      :: {binary(), binary()}.
-type ice_candidate_extension_attr_list() :: [ice_candidate_extension_attr()].

-type parse_result()  :: ersip_parser_aux:parse_result(ice_candidate()).
-type parse_result(X) :: ersip_parser_aux:parse_result(X).

%%%===================================================================
%%% API
%%%===================================================================

-spec foundation(ice_candidate()) -> ice_candidate_foundation().
foundation(#ice_candidate{foundation = F}) ->
    F.

-spec component_id(ice_candidate()) -> ice_candidate_component_id().
component_id(#ice_candidate{component_id = CI}) ->
    CI.

-spec transport(ice_candidate()) -> binary().
transport(#ice_candidate{transport = T}) ->
    T.

-spec priority(ice_candidate()) -> pos_integer().
priority(#ice_candidate{priority = P}) ->
    P.

-spec connection_address(ice_candidate()) -> ersip_sdp_addr:addr().
connection_address(#ice_candidate{connection_address = CA}) ->
    CA.

-spec set_connection_address(ersip_sdp_addr:addr(), ice_candidate()) -> ice_candidate().
set_connection_address(CA, #ice_candidate{} = IceCandidate) ->
    IceCandidate#ice_candidate{connection_address = CA}.

-spec port(ice_candidate()) -> inet:port_number().
port(#ice_candidate{port = P}) ->
    P.

-spec cand_type(ice_candidate()) -> ice_candidate_cand_type().
cand_type(#ice_candidate{cand_type = CT}) ->
    CT.

-spec rel_addr(ice_candidate()) -> ersip_sdp_addr:addr() | undefined.
rel_addr(#ice_candidate{rel_addr = RA}) ->
    RA.

-spec set_rel_addr(ersip_sdp_addr:addr(), ice_candidate()) -> ice_candidate().
set_rel_addr(RA, #ice_candidate{} = IceCandidate) ->
    IceCandidate#ice_candidate{rel_addr = RA}.

-spec rel_port(ice_candidate()) -> inet:port_number() | undefined.
rel_port(#ice_candidate{rel_port = RP}) ->
    RP.

-spec attrs(ice_candidate()) -> ice_candidate_extension_attr_list() | undefined.
attrs(#ice_candidate{attrs = A}) ->
    A.


-spec parse(binary()) -> parse_result().
parse(Bin) ->
    do_parse_ice_candidate(Bin).

-spec assemble(ice_candidate()) -> iolist().
assemble(IceCandidate) ->
    assemble_ice_candidate(IceCandidate).

-spec assemble_bin(ice_candidate()) -> binary().
assemble_bin(IceCandidate) ->
    iolist_to_binary(assemble(IceCandidate)).

%%%===================================================================
%%% Internal implementation
%%%===================================================================

-define(sp, " ").

%% https://tools.ietf.org/html/rfc5245#section-15
%%
%%  candidate-attribute   = "candidate" ":"
%%                           foundation SP
%%                           component-id SP
%%                           transport SP
%%                           priority SP
%%                           connection-address SP ;from RFC 4566
%%                           port SP               ;from RFC 4566
%%                           cand-type
%%                           [SP rel-addr]
%%                           [SP rel-port]
%%                           *(SP extension-att-name SP
%%                                extension-att-value)
%%
-spec do_parse_ice_candidate(binary()) -> parse_result().
do_parse_ice_candidate(<<Rest/binary>>) ->
    Parsers = [fun ersip_sdp_aux:parse_token/1,             %% foundation
               fun ersip_parser_aux:parse_lws/1,
               fun ersip_parser_aux:parse_pos_int/1,        %% component-id
               fun ersip_parser_aux:parse_lws/1,
               fun ersip_sdp_aux:parse_token/1,             %% transport
               fun ersip_parser_aux:parse_lws/1,
               fun ersip_parser_aux:parse_pos_int/1,        %% priority
               fun ersip_parser_aux:parse_lws/1,
               fun connection_address_parse/1,              %% connection-address
               fun ersip_parser_aux:parse_lws/1,
               fun ersip_parser_aux:parse_non_neg_int/1,    %% port
               fun ersip_parser_aux:parse_lws/1,
               fun cand_type_parse/1,                       %% cand-type
               fun rel_addr_parse/1,                        %% rel-addr
               fun rel_port_parse/1,                        %% rel-port
               fun extension_attr_parse/1
              ],
    case ersip_parser_aux:parse_all(Rest, Parsers) of
        {ok, Result, Rest1} ->
            [Foundation, _, ComponentId, _, Transport, _, Priority, _, ConnectionAddress, _, Port, _, CandType,
             RelAddr,
             RelPort,
             Attrs] = Result,
            IceCandidate = #ice_candidate{foundation         = Foundation,
                                          component_id       = ComponentId,
                                          transport          = Transport,
                                          priority           = Priority,
                                          connection_address = ConnectionAddress,
                                          port               = Port,
                                          cand_type          = CandType,
                                          rel_addr           = RelAddr,
                                          rel_port           = RelPort,
                                          attrs              = Attrs},
            {ok, IceCandidate, Rest1};
        {error, Reason} ->
            {error, {invalid_ice_candidate, Reason}}
    end.

-spec assemble_ice_candidate(ice_candidate()) -> iolist().
assemble_ice_candidate(#ice_candidate{} = IceCandidate) ->
    #ice_candidate{foundation         = Foundation,
                   component_id       = ComponentId,
                   transport          = Transport,
                   priority           = Priority,
                   connection_address = ConnectionAddress,
                   port               = Port,
                   cand_type          = CandType,
                   rel_addr           = RelAddr,
                   rel_port           = RelPort,
                   attrs              = Attrs} = IceCandidate,
    [Foundation, ?sp,
     integer_to_binary(ComponentId), ?sp,
     Transport, ?sp,
     integer_to_binary(Priority), ?sp,
     connection_address_assemble(ConnectionAddress), ?sp,
     integer_to_binary(Port), ?sp,
     cand_type_assemble(CandType),
     rel_addr_assemble(RelAddr),
     rel_port_assemble(RelPort),
     extension_attr_assemble(Attrs)].


-spec connection_address_parse(binary()) -> parse_result(ersip_sdp_addr:addr()).
connection_address_parse(Bin) ->
    {ok, AddrBin, Rest} = parse_address(Bin),
    case ersip_sdp_addr:parse(<<"in">>, <<"ip4">>, AddrBin) of
        {ok, {ip4, _} = Addr} ->
            {ok, Addr, Rest};
        {ok, {ip4_host, _} = Domain} ->
            {ok, Domain, Rest};
        {error, _} ->
            case ersip_sdp_addr:parse(<<"in">>, <<"ip6">>, AddrBin) of
                {ok, {ip6, _} = Addr} ->
                    {ok, Addr, Rest};
                {ok, {ip6_host, _} = Domain} ->
                    {ok, Domain, Rest};
                {error, _} ->
                    {error, invalid_address}
            end
    end.

-spec connection_address_assemble(ersip_sdp_addr:addr()) -> binary().
connection_address_assemble({ip4, IP4}) ->
    list_to_binary(inet:ntoa(IP4));
connection_address_assemble({ip6, IP6}) ->
    list_to_binary(inet:ntoa(IP6));
connection_address_assemble({ip4_host, Domain}) ->
    Domain;
connection_address_assemble({ip6_host, Domain}) ->
    Domain.


-spec cand_type_parse(binary()) -> parse_result(ice_candidate_cand_type()).
cand_type_parse(<<"typ", ?sp, Rest/binary>>) ->
    ersip_sdp_aux:parse_token(Rest).

-spec cand_type_assemble(ice_candidate_cand_type()) -> binary().
cand_type_assemble(Type) ->
    <<"typ", ?sp, Type/binary>>.


-spec rel_addr_parse(binary()) -> parse_result(ersip_sdp_addr:addr() | undefined).
rel_addr_parse(<<>>) ->
    {ok, undefined, <<>>};

rel_addr_parse(<<?sp, "raddr", ?sp, Rest/binary>>) ->
    connection_address_parse(Rest);

rel_addr_parse(Rest) ->
    {ok, undefined, Rest}.

-spec rel_addr_assemble(ersip_sdp_addr:addr() | undefined) -> iolist().
rel_addr_assemble(undefined) ->
    [];

rel_addr_assemble(RelAddr) ->
    RelAddrBin = connection_address_assemble(RelAddr),
    [?sp, "raddr", ?sp, RelAddrBin].


-spec rel_port_parse(binary()) -> parse_result(inet:port_number() | undefined).
rel_port_parse(<<>>) ->
    {ok, undefined, <<>>};

rel_port_parse(<<?sp, "rport", ?sp, Rest/binary>>) ->
    ersip_parser_aux:parse_non_neg_int(Rest);

rel_port_parse(Rest) ->
    {ok, undefined, Rest}.

-spec rel_port_assemble(inet:port_number() | undefined) -> iolist().
rel_port_assemble(undefined) ->
    [];

rel_port_assemble(RelPort) ->
    RelPortBin = integer_to_binary(RelPort),
    [?sp, "rport", ?sp, RelPortBin].


-spec extension_attr_parse(binary()) -> parse_result(ice_candidate_extension_attr_list()).
extension_attr_parse(<<>>) ->
    {ok, [], <<>>};

extension_attr_parse(<<?sp, Rest/binary>>) ->
    Xs = binary:split(Rest, <<?sp>>, [trim_all, global]),
    case length(Xs) rem 2 == 0 of
        true ->
            Attrs = extension_attr_compose(Xs, []),
            {ok, Attrs, <<>>};
        false ->
            {error, {invalid_extension_attr, no_pair}}
    end.

-spec extension_attr_assemble(ice_candidate_extension_attr_list()) -> iolist().
extension_attr_assemble(Attrs) ->
    [[?sp, K, ?sp, V] || {K, V} <- Attrs].

%%%===================================================================
%%% Helpers
%%%===================================================================

-spec parse_address(binary()) -> parse_result(binary()).
parse_address(Bin) ->
    AddrEnd = find_addr_end(Bin, 0),
    <<Addr:AddrEnd/binary, Rest/binary>> = Bin,
    {ok, Addr, Rest}.

-spec find_addr_end(binary(), non_neg_integer()) -> non_neg_integer().
find_addr_end(<<>>, Acc) ->
    Acc;
find_addr_end(<<?sp, _/binary>>, Acc) ->
    Acc;
find_addr_end(<<_:8, Rest/binary>>, Acc) ->
    find_addr_end(Rest, Acc+1).

-spec extension_attr_compose(list(binary()), ice_candidate_extension_attr_list()) -> ice_candidate_extension_attr_list().
extension_attr_compose([], Acc) ->
    lists:reverse(Acc);
extension_attr_compose([X, Y | Rest], Acc) ->
    extension_attr_compose(Rest, [{X, Y} | Acc]).

