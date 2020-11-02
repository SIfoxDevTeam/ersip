%%%
%%% SIP History-Info header (single header).
%%%

-module(ersip_hdr_history_info).

-export([parse/1,
         display_name/1,
         uri/1,
         params/1,
         param/2,
         parse_hdr/1
        ]).

-export_type([history_info/0]).

%%===================================================================
%% Types
%%===================================================================

-record(history_info, {display_name :: ersip_nameaddr:display_name(),
                       uri          :: ersip_uri:uri(),
                       hparams   = ersip_hparams:new() :: ersip_hparams:hparams()
               }).
-type history_info()        :: #history_info{}.
-type history_info_param()  :: {Key :: binary(), Value :: binary()}.
-type parse_result() :: {ok, history_info()} | {error, parse_error()}.
-type parse_error()  :: {invalid_history_info, term()}.

%%===================================================================
%% API
%%===================================================================

%% @doc URI from History-Info header.
-spec uri(history_info()) -> ersip_uri:uri().
uri(#history_info{uri = URI}) ->
    URI.

%% @doc Display name in History-Info header.
-spec display_name(history_info()) -> ersip_display_name:display_name().
display_name(#history_info{display_name = DN}) ->
    DN.

%% @doc Get parameters of History-Info header.
-spec params(history_info()) -> [history_info_param()].
params(#history_info{hparams = HP}) ->
    ersip_hparams:to_raw_list(HP).

%% @doc Get parameter of History-Info header.
-spec param(binary(), history_info()) -> {ok, binary()} | not_found.
param(Key, #history_info{hparams = HParams}) ->
    ersip_hparams:find_raw(Key, HParams).

%% @doc Parse History-Info header from binary.
-spec parse(binary()) -> parse_result().
parse(Bin) ->
    case parse_hdr(Bin) of
        {ok, HistoryInfo, <<>>} -> {ok, HistoryInfo};
        {ok, _, _} -> {error, {invalid_history_info, Bin}};
        {error, Reason} -> {error, {invalid_history_info, Reason}}
    end.

%% @doc Parse single History-Info header and return unparsed rest.
-spec parse_hdr(binary()) -> ersip_parser_aux:parse_result(history_info()).
parse_hdr(Bin) ->
    Parsers = [fun ersip_nameaddr:parse/1,
               fun ersip_parser_aux:trim_lws/1,
               fun parse_params/1,
               fun ersip_parser_aux:trim_lws/1
              ],
    case ersip_parser_aux:parse_all(Bin, Parsers) of
        {ok, [{DisplayName, URI}, _, HParams, _], Rest} ->
            HistoryInfo = #history_info{display_name = DisplayName,
                           uri = URI,
                           hparams = HParams},
            {ok, HistoryInfo, Rest};
        {error, Reason} ->
            {error, {invalid_history_info, Reason}}
    end.

%%===================================================================
%% Helpers
%%===================================================================

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
