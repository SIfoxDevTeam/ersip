%%%
%%% Reusable general hdr. One SIP name-addr / addr-spec entry (e.g. P-Served-User, P-Asserted-Identity)
%%%

-module(ersip_hdr_name_addr).

-export([new/1,
         uri/1,
         set_uri/2,
         display_name/1,
         set_display_name/2,
         params/1,
         param/2,
         set_param/3,
         make/1,
         assemble/1,
         assemble_bin/1,
         parse_hdr/1,
         raw/1
        ]).

-export_type([hdr_name_addr/0, raw/0]).

%%===================================================================
%% Types
%%===================================================================

-record(hdr_name_addr, {display_name :: ersip_nameaddr:display_name(),
                        uri          :: ersip_uri:uri(),
                        hparams   = ersip_hparams:new() :: ersip_hparams:hparams()
                       }).
-type hdr_name_addr():: #hdr_name_addr{}.
-type hdr_param()  :: {Key :: binary(), Value :: binary()}.
-type parse_result() :: {ok, hdr_name_addr()} | {error, parse_error()}.
-type parse_error()  :: {invalid_hdr_name_addr, term()}.
-type raw() :: #{uri          := ersip_uri:raw(),
                 params       := ersip_hparams:raw(),
                 display_name := ersip_display_name:raw()}.

%%===================================================================
%% API
%%===================================================================

%% @doc Create a header from SIP URI.
-spec new(ersip_uri:uri()) -> hdr_name_addr().
new(URI) ->
    #hdr_name_addr{display_name = {display_name, []}, uri = URI}.

%% @doc URI from a header.
-spec uri(hdr_name_addr()) -> ersip_uri:uri().
uri(#hdr_name_addr{uri = URI}) ->
    URI.

%% @doc Set URI of a header.
-spec set_uri(ersip_uri:uri(), hdr_name_addr()) -> hdr_name_addr().
set_uri(URI, #hdr_name_addr{} = R) ->
    R#hdr_name_addr{uri = URI}.

%% @doc Display name in a header.
-spec display_name(hdr_name_addr()) -> ersip_display_name:display_name().
display_name(#hdr_name_addr{display_name = DN}) ->
    DN.

%% @doc Set display name of a header.
-spec set_display_name(ersip_display_name:display_name(), hdr_name_addr()) -> hdr_name_addr().
set_display_name(DN, #hdr_name_addr{} = Hdr) ->
    Hdr#hdr_name_addr{display_name = DN}.

%% @doc Get parameters of a header.
-spec params(hdr_name_addr()) -> [hdr_param()].
params(#hdr_name_addr{hparams = HP}) ->
    ersip_hparams:to_raw_list(HP).

%% @doc Get parameter of a header.
-spec param(binary(), hdr_name_addr()) -> {ok, binary()} | not_found.
param(Key, #hdr_name_addr{hparams = HParams}) ->
    ersip_hparams:find_raw(Key, HParams).

%% @doc Set parameters of a header.
-spec set_param(Key :: binary(), Value :: binary(), hdr_name_addr()) -> hdr_name_addr().
set_param(Key, Value, #hdr_name_addr{hparams = HParams} = Hdr)
        when is_binary(Key), is_binary(Value) ->
    Hdr#hdr_name_addr{hparams = ersip_hparams:set_raw(Key, Value, HParams)}.

%% @doc Make a header from binary or from raw representation.
-spec make(binary()) -> hdr_name_addr().
make(Bin) when is_binary(Bin) ->
    case parse(Bin) of
        {ok, Hdr} -> Hdr;
        {error, Reason} -> error(Reason)
    end;
make(#{uri := URI} = Raw) ->
    HParams0 = ersip_hparams:make(maps:get(params, Raw, #{})),
    HParams =
        case ersip_hparams:parse_known(fun parse_known/2, HParams0) of
            {ok, H} -> H
            %% {error, Reason} -> error({invalid_params, Reason}) %% (Ref1) see below
        end,
    #hdr_name_addr{display_name  = ersip_display_name:make(maps:get(display_name, Raw, <<>>)),
           uri           = ersip_uri:make(URI),
           hparams       = HParams
          }.

%% @doc Parse a header from binary.
-spec parse(binary()) -> parse_result().
parse(Bin) ->
    case parse_hdr(Bin) of
        {ok, Hdr, <<>>} -> {ok, Hdr};
        {ok, _, _} -> {error, {invalid_hdr_name_addr, Bin}};
        {error, Reason} -> {error, {invalid_hdr_name_addr, Reason}}
    end.

%% @doc Parse single a header and return unparsed rest.
-spec parse_hdr(binary()) -> ersip_parser_aux:parse_result(hdr_name_addr()).
parse_hdr(Bin) ->
    Parsers = [fun ersip_nameaddr:parse/1,
               fun ersip_parser_aux:trim_lws/1,
               fun parse_params/1,
               fun ersip_parser_aux:trim_lws/1
              ],
    case ersip_parser_aux:parse_all(Bin, Parsers) of
        {ok, [{DisplayName, URI}, _, HParams, _], Rest} ->
            Hdr = #hdr_name_addr{display_name = DisplayName,
                           uri = URI,
                           hparams = HParams},
            {ok, Hdr, Rest};
        {error, Reason} ->
            {error, {invalid_hdr_name_addr, Reason}}
    end.

%% @doc Assemble a header to iolist().
-spec assemble(hdr_name_addr()) -> iolist().
assemble(#hdr_name_addr{display_name = DN, uri = URI, hparams = HParams}) ->
    HParamsIO0 = ersip_hparams:assemble(HParams),
    HParamsIO =
        case ersip_iolist:is_empty(HParamsIO0) of
            true -> [];
            false -> [$; | HParamsIO0]
        end,
    [ersip_nameaddr:assemble(DN, URI), HParamsIO].

%% @doc Assemble a header to binary().
-spec assemble_bin(hdr_name_addr()) -> binary().
assemble_bin(#hdr_name_addr{} = R) ->
    iolist_to_binary(assemble(R)).

%% @doc Raw representation of a header.
-spec raw(hdr_name_addr()) -> raw().
raw(#hdr_name_addr{} = Hdr) ->
    #{uri => ersip_uri:raw(uri(Hdr)),
      display_name => ersip_display_name:raw(display_name(Hdr)),
      params => ersip_hparams:raw(Hdr#hdr_name_addr.hparams)
     }.

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
