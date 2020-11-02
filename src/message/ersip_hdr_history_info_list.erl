%%%
%%% SIP History-Info headers
%%%

-module(ersip_hdr_history_info_list).

-export([parse/1]).

-include("ersip_headers.hrl").

%%===================================================================
%% Types
%%===================================================================

-type history_info_list() :: [ersip_hdr_history_info:history_info()].
-type maybe_rev_history_info_list() :: {ok, history_info_list()} | {error, term()}.
-type parse_result() :: {ok, history_info_list()}
                      | {error, term()}.

%%===================================================================
%% API
%%===================================================================

-spec parse(ersip_hdr:header()) -> parse_result().
parse(Header) ->
    MaybeRevHistoryInfoList =
        lists:foldl(fun(IOHistoryInfo, Acc) ->
            add_to_maybe_history_info_list(iolist_to_binary(IOHistoryInfo), Acc)
                    end,
            {ok, []},
            ersip_hdr:raw_values(Header)),
    case MaybeRevHistoryInfoList of
        {ok, RevHistoryInfoList} ->
            {ok, lists:reverse(RevHistoryInfoList)};
        Error ->
            Error
    end.


%%===================================================================
%% Internal implementation
%%===================================================================

%% @private
-spec add_to_maybe_history_info_list(binary(), maybe_rev_history_info_list()) -> maybe_rev_history_info_list().
add_to_maybe_history_info_list(_, {error, _} = Error) ->
    Error;
add_to_maybe_history_info_list(<<>>, {ok, _} = Result) ->
    Result;
add_to_maybe_history_info_list(Bin, {ok, HistoryInfoList}) when is_list(HistoryInfoList) ->
    case ersip_hdr_history_info:parse_hdr(Bin) of
        {ok, HistoryInfo, Rest0} ->
            case ersip_bin:trim_head_lws(Rest0) of
                <<>> ->
                    {ok, [HistoryInfo | HistoryInfoList]};
                <<",", Rest1/binary>> ->
                    Rest2 = ersip_bin:trim_head_lws(Rest1),
                    add_to_maybe_history_info_list(Rest2, {ok, [HistoryInfo | HistoryInfoList]});
                <<C, _/binary>> ->
                    {error, {unexpected_separator, C}}
            end;
        {error, _} = Error ->
            Error
    end.
