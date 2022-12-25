%%%
%%% Reusable general hdr. SIP name-addr / addr-spec list (e.g. P-Asserted-Identity)
%%%

-module(ersip_hdr_name_addr_list).

-export([parse/1, build/2]).

-include("ersip_headers.hrl").

%%===================================================================
%% Types
%%===================================================================

-type name_addr_list() :: [ersip_hdr_name_addr:hdr_name_addr()].
-type maybe_rev_name_addr_list() :: {ok, name_addr_list()} | {error, term()}.
-type parse_result() :: {ok, name_addr_list()}
                      | {error, term()}.

%%===================================================================
%% API
%%===================================================================

-spec parse(ersip_hdr:header()) -> parse_result().
parse(Header) ->
    MaybeRevHdrNameAddrList =
        lists:foldl(fun(IOHdrNameAddr, Acc) ->
            add_to_maybe_name_addr_list(iolist_to_binary(IOHdrNameAddr), Acc)
                    end,
            {ok, []},
            ersip_hdr:raw_values(Header)),
    case MaybeRevHdrNameAddrList of
        {ok, RevHdrNameAddrList} ->
            {ok, lists:reverse(RevHdrNameAddrList)};
        Error ->
            Error
    end.

%% @doc Build raw SIP header.
-spec build(HeaderName :: binary(), name_addr_list()) -> ersip_hdr:header().
build(HdrName, HdrNameAddrList) when is_list(HdrNameAddrList) ->
    Hdr = ersip_hdr:new(HdrName),
    lists:foldl(
        fun(HdrNameAddr, HdrAcc) ->
            ersip_hdr:add_value(ersip_hdr_name_addr:assemble(HdrNameAddr), HdrAcc)
        end,
        Hdr,
        HdrNameAddrList).

%%===================================================================
%% Internal implementation
%%===================================================================

%% @private
-spec add_to_maybe_name_addr_list(binary(), maybe_rev_name_addr_list()) -> maybe_rev_name_addr_list().
add_to_maybe_name_addr_list(_, {error, _} = Error) ->
    Error;
add_to_maybe_name_addr_list(<<>>, {ok, _} = Result) ->
    Result;
add_to_maybe_name_addr_list(Bin, {ok, HdrNameAddrList}) when is_list(HdrNameAddrList) ->
    case ersip_hdr_name_addr:parse_hdr(Bin) of
        {ok, HdrNameAddr, Rest0} ->
            case ersip_bin:trim_head_lws(Rest0) of
                <<>> ->
                    {ok, [HdrNameAddr | HdrNameAddrList]};
                <<",", Rest1/binary>> ->
                    Rest2 = ersip_bin:trim_head_lws(Rest1),
                    add_to_maybe_name_addr_list(Rest2, {ok, [HdrNameAddr | HdrNameAddrList]});
                <<C, _/binary>> ->
                    {error, {unexpected_separator, C}}
            end;
        {error, _} = Error ->
            Error
    end.
