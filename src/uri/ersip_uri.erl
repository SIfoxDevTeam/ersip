%%%
%%% Copyright (c) 2017, 2018, 2019, 2020 Dmitry Poroh
%%% All rights reserved.
%%% Distributed under the terms of the MIT License. See the LICENSE file.
%%%
%%% SIP URI
%%%

-module(ersip_uri).

-export([scheme/1,
         scheme_bin/1,
         data/1,

         user/1,
         set_user/2,

         host/1,
         host_bin/1,
         set_host/2,

         port/1,
         set_port/2,

         transport/1,
         set_transport/2,
         clear_transport/1,

         loose_router/1,
         set_loose_router/2,

         maddr/1,
         set_maddr/2,
         clear_maddr/1,

         user_param/1,
         set_user_param/2,
         clear_user_param/1,

         ttl/1,
         set_ttl/2,
         clear_ttl/1,

         gen_param/2,
         set_gen_param/3,
         clear_gen_param/2,

         get/2,
         make/1,
         make_key/1,
         parse/1,
         assemble/1,
         assemble_bin/1,
         params/1,
         raw_params/1,
         raw_headers/1,
         set_raw_headers/2,
         clear_params/1,
         set_param/3,
         clear_not_allowed_parts/2,
         rebuild_header_values/1,
         assemble_scheme/1,
         is_sip/1,
         raw/1
        ]).
-export_type([uri/0, scheme/0, raw/0]).

-include("ersip_sip_abnf.hrl").

%%===================================================================
%% Types
%%===================================================================

-record(sip_uri_data, {
          %% user: The identifier of a particular resource at the host being
          %%    addressed.  The term "host" in this context frequently refers
          %%    to a domain.  The "userinfo" of a URI consists of this user
          %%    field, the password field, and the @ sign following them.  The
          %%    userinfo part of a URI is optional and MAY be absent when the
          %%    destination host does not have a notion of users or when the
          %%    host itself is the resource being identified.  If the @ sign is
          %%    present in a SIP or SIPS URI, the user field MUST NOT be empty.
          user   = undefined :: undefined
                              | {user, binary()},
          %% host: The host providing the SIP resource.  The host part contains
          %%    either a fully-qualified domain name or numeric IPv4 or IPv6
          %%    address.  Using the fully-qualified domain name form is
          %%    RECOMMENDED whenever possible.
          host               :: ersip_host:host(),
          %% original (binary) representation of host
          host_orig          :: binary() | undefined,
          %% port: The port number where the request is to be sent.
          port               :: undefined | inet:port_number(),
          %% URI parameters: Parameters affecting a request constructed from
          %% the URI.
          params = #{}       :: uri_params(),
          %% URI headers
          headers = #{}      :: uri_headers()
         }).
-record(tel_uri_data, {
    user   = undefined :: undefined | {user, binary()},
    params = #{}       :: uri_params(),
    headers = #{}      :: uri_headers()
}).
-record(absolute_uri_data, {opaque :: binary()}).
-record(uri, {scheme = {scheme, sip}   :: scheme(),
              data   = #sip_uri_data{} :: uri_data()
             }).


-type sip_uri_data()      :: #sip_uri_data{}.
-type tel_uri_data()      :: #tel_uri_data{}.
-type absolute_uri_data() :: #absolute_uri_data{}.
-type uri()               :: #uri{}.

-type uri_data()   :: sip_uri_data() | absolute_uri_data() | tel_uri_data().

-type uri_params() :: #{transport => ersip_transport:transport(),
                        maddr     => ersip_host:host(),
                        ttl       => 0..255,
                        user      => phone | ip | binary(),
                        method    => binary(),
                        lr        => true
                       }.
-type uri_headers() ::  #{binary() => binary()}.
-type scheme()   :: {scheme, sip | sips | tel | binary()}.

-type uri_param_name() :: known_param() | binary().

-type uri_part_name() :: scheme | user | host | port.
-type uri_part() :: scheme()  | {user, binary()} | {host, ersip_host:host()}
                  | {port, inet:port_number() | undefined}.

-type raw() :: #{scheme := binary(),
                 data := binary(),
                 sip => sip_uri_raw()}.

-type sip_uri_raw() :: #{host := binary(),
                         user => binary(),
                         port => inet:port_number(),
                         params => key_value_list(),
                         headers => key_value_list()
                        }.
-type key_value_list() :: [{binary(), binary()} | binary()].

-type parse_result() :: {ok, uri()} | {error, parse_error()}.
-type parse_error() :: {invalid_scheme, binary()}
                     | {invalid_sip_uri, sip_uri_parse_error()}.
-type sip_uri_parse_result() :: {ok, sip_uri_data()} | {error, sip_uri_parse_error()}.
-type sip_uri_parse_error() :: {invalid_host, ersip_host:parse_error() | {garbage_at_the_end, binary()}}
                             | {invalid_ipv6_reference, binary()}
                             | {invalid_port, binary()}.
-type tel_uri_parse_result() :: {ok, tel_uri_data()} | {error, term()}.

-type known_param() :: transport | lr | ttl | user | maddr.
-type known_param_value() :: ersip_transport:transport()
                           | boolean()
                           | ttl_param()
                           | user_param()
                           | ersip_host:host().
-type user_param() :: phone | ip | binary().
-type ttl_param() :: 0..255.

%%===================================================================
%% API
%%===================================================================
-define(IS_VALID_PORT(X), is_integer(P), P >= 0, P =< 65535; P == undefined).

%% @doc Scheme of the URI.
-spec scheme(uri()) -> scheme().
scheme(#uri{scheme = S}) ->
    S.

%% @doc URI scheme in binary form.
-spec scheme_bin(uri()) -> binary().
scheme_bin(#uri{scheme = {scheme, sip}}) ->
    <<"sip">>;
scheme_bin(#uri{scheme = {scheme, sips}}) ->
    <<"sips">>;
scheme_bin(#uri{scheme = {scheme, tel}}) ->
    <<"tel">>;
scheme_bin(#uri{scheme = {scheme, Bin}}) when is_binary(Bin) ->
    ersip_bin:to_lower(Bin).

%% @doc Get data of the URI (everything after scheme).
%% Example:
%% ```
%%    <<"+16505550505">> = ersip_uri:data(ersip_uri:make(<<"tel:+16505550505">>)).
%%    <<"a@b">> = ersip_uri:data(ersip_uri:make(<<"sip:a@b">>)).
%% '''
-spec data(uri()) -> binary().
data(#uri{data = Data}) ->
    iolist_to_binary(assemble_data(Data)).

%% @doc Get user part of SIP URI.
%% Raises error if URI is not SIP(S) or TEL URI.
%% Example:
%% ```
%%    <<"alice">> = ersip_uri:user(ersip_uri:make(<<"sip:alice@atlanta.com">>)).
%%    <<"bob">> = ersip_uri:user(ersip_uri:make(<<"sips:bob@biloxi.com">>)).
%%    undefined = ersip_uri:user(ersip_uri:make(<<"sip:biloxi.com">>)).
%%    ersip_uri:user(ersip_uri:make(<<"tel:+16505550505">>)). % raises error
%% '''
-spec user(ersip_uri:uri()) -> binary() | undefined.
user(#uri{data = #sip_uri_data{user = undefined}}) ->
    undefined;
user(#uri{data = #sip_uri_data{user = {user, U}}}) ->
    U;
user(#uri{data = #tel_uri_data{user = {user, U}}}) ->
    U;
user(#uri{} = URI) ->
    error({sip_uri_expected, URI}).


%% @doc Set user part of SIP URI.
%% Raises error if URI is not SIP(S) or TEL URI.
-spec set_user(binary(), ersip_uri:uri()) -> ersip_uri:uri().
set_user(NewUser, #uri{data = #sip_uri_data{} = D} = U) when is_binary(NewUser) ->
    U#uri{data = D#sip_uri_data{user = {user, NewUser}}};
set_user(NewUser, #uri{data = #tel_uri_data{} = D} = U) when is_binary(NewUser) ->
    U#uri{data = D#tel_uri_data{user = {user, NewUser}}};
set_user(NewUser, #uri{} = URI) when is_binary(NewUser)  ->
    error({sip_uri_expected, URI}).

%% @doc Get host part of SIP URI.
%% Raises error if URI is not SIP(S) URI.
-spec host(ersip_uri:uri()) -> ersip_host:host().
host(#uri{data = #sip_uri_data{host = H}}) ->
    H;
host(#uri{} = URI) ->
    error({sip_uri_expected, URI}).

%% @doc Get host part of SIP URI in binary representation.
%% Raises error if URI is not SIP(S) URI.
%% Example:
%% ```
%%    <<"atlanta.com">> = ersip_uri:host_bin(ersip_uri:make(<<"sip:alice@atlanta.com">>)).
%%    <<"biloxi.com">> = ersip_uri:host_bin(ersip_uri:make(<<"sips:bob@biloxi.com">>)).
%%    <<"127.0.0.1">> = ersip_uri:host_bin(ersip_uri:make(<<"sip:127.0.0.1">>)).
%%    ersip_uri:host_bin(ersip_uri:make(<<"tel:+16505550505">>)). % raises error
%% '''
-spec host_bin(ersip_uri:uri()) -> binary().
host_bin(#uri{data = #sip_uri_data{host_orig = undefined, host = H}}) ->
    ersip_host:assemble_bin(H);
host_bin(#uri{data = #sip_uri_data{host_orig = HostBin}}) when is_binary(HostBin) ->
    HostBin;
host_bin(#uri{} = URI) ->
    error({sip_uri_expected, URI}).

-spec set_host(ersip_host:host(), ersip_uri:uri()) -> ersip_uri:uri().
set_host(H, #uri{data = #sip_uri_data{host = H}} = U) ->
    U;
set_host(H, #uri{data = #sip_uri_data{} = D} = U) ->
    U#uri{data = D#sip_uri_data{host = H, host_orig = undefined}};
set_host(_, #uri{} = URI) ->
    error({sip_uri_expected, URI}).

%% @doc Get port number of 'undefined'.
%% Raises error if URI is not SIP(S) URI.
%% ```
%%    undefined = ersip_uri:port(ersip_uri:make(<<"sip:alice@atlanta.com">>)).
%%    5060 = ersip_uri:port(ersip_uri:make(<<"sip:alice@atlanta.com:5060">>)).
%%    ersip_uri:port(ersip_uri:make(<<"tel:+16505550505">>)). %% raises error
%% '''
-spec port(ersip_uri:uri()) -> undefined | inet:port_number().
port(#uri{data = #sip_uri_data{port = P}}) ->
    P;
port(#uri{} = URI) ->
    error({sip_uri_expected, URI}).

%% @doc Set port number.
%% Raises error if URI is not SIP(S) URI.
-spec set_port(undefined | inet:port_number(), ersip_uri:uri()) -> ersip_uri:uri().
set_port(P, #uri{data = #sip_uri_data{} = D} = U) when ?IS_VALID_PORT(P) ->
    U#uri{data = D#sip_uri_data{port = P}};
set_port(P, #uri{} = URI) when ?IS_VALID_PORT(P) ->
    error({sip_uri_expected, URI}).

%% @doc Get transport from URI.
%% Raises error if URI is not SIP(S) or TEL URI.
-spec transport(ersip_uri:uri()) -> undefined | ersip_transport:transport().
transport(#uri{data = #sip_uri_data{params = #{transport := T}}}) ->
    T;
transport(#uri{data = #sip_uri_data{params = #{}}}) ->
    undefined;
transport(#uri{data = #tel_uri_data{params = #{transport := T}}}) ->
    T;
transport(#uri{data = #tel_uri_data{params = #{}}}) ->
    undefined;
transport(#uri{} = URI) ->
    error({sip_uri_expected, URI}).

%% @doc Set transport to URI.
%% Raises error if URI is not SIP(S) URI.
-spec set_transport(ersip_transport:transport(), uri()) -> uri().
set_transport(Transport, #uri{} = URI) ->
    set_sip_param(transport, Transport, URI).

%% @doc Remove transport parameter from URI.
%% Raises error if URI is not SIP(S) URI.
-spec clear_transport(uri()) -> uri().
clear_transport(#uri{} = URI) ->
    clear_sip_param(transport, URI).

%% @doc Checks if URI has loose router parameter (lr).
%% Raises error if URI is not SIP(S) or TEL URI.
%% Example:
%% ```
%%   true  = ersip_uri:loose_route(ersip_uri:make(<<"sip:host;lr">>)).
%%   false = ersip_uri:loose_route(ersip_uri:make(<<"sip:host">>)).
%% '''
-spec loose_router(uri()) -> boolean().
loose_router(#uri{data = #sip_uri_data{params = #{lr := true}}}) ->
    true;
loose_router(#uri{data = #sip_uri_data{}}) ->
    false;
loose_router(#uri{data = #tel_uri_data{params = #{lr := true}}}) ->
    true;
loose_router(#uri{data = #tel_uri_data{}}) ->
    false;
loose_router(#uri{} = URI) ->
    error({sip_uri_expected, URI}).

-spec set_loose_router(boolean(), uri()) -> uri().
set_loose_router(true, #uri{} = URI) ->
    set_sip_param(lr, true, URI);
set_loose_router(false, #uri{} = URI) ->
    clear_sip_param(lr, URI).

%% @doc Return maddr parameter value or undefined.
%% Raises error if URI is not SIP(S) or TEL URI.
-spec maddr(uri()) -> ersip_host:host() | undefined.
maddr(#uri{data = #sip_uri_data{params = #{maddr := Maddr}}}) ->
    Maddr;
maddr(#uri{data = #sip_uri_data{}}) ->
    undefined;
maddr(#uri{data = #tel_uri_data{params = #{maddr := Maddr}}}) ->
    Maddr;
maddr(#uri{data = #tel_uri_data{}}) ->
    undefined;
maddr(#uri{} = URI) ->
    error({sip_uri_expected, URI}).

%% @doc Set maddr parameter value.
%% Raises error if URI is not SIP(S) URI or if first parameter is not
%% host.
-spec set_maddr(ersip_host:host(), uri()) -> uri().
set_maddr(Host, #uri{} = URI) ->
    case ersip_host:is_host(Host) of
        true ->
            set_sip_param(maddr, Host, URI);
        false ->
            error({host_expected, Host})
    end.

%% @doc Remove maddr parameter from URI.
%% Raises error if URI is not SIP(S) URI.
-spec clear_maddr(uri()) -> uri().
clear_maddr(#uri{} = URI) ->
    clear_sip_param(maddr, URI).

%% @doc Get user parameter of URI (Ex: ;user=ip or ;user=phone).
%% Raises error if URI is not SIP(S) or TEL URI.
-spec user_param(uri()) -> user_param() | undefined.
user_param(#uri{data = #sip_uri_data{params = #{user := U}}}) ->
    U;
user_param(#uri{data = #sip_uri_data{}}) ->
    undefined;
user_param(#uri{data = #tel_uri_data{params = #{user := U}}}) ->
    U;
user_param(#uri{data = #tel_uri_data{}}) ->
    undefined;
user_param(#uri{} = URI) ->
    error({sip_uri_expected, URI}).

%% @doc Set user parameter of URI.
%% Raises error if URI is not SIP(S) URI.
-spec set_user_param(user_param(), uri()) -> uri().
set_user_param(UserParam, #uri{} = URI)
  when UserParam == ip; UserParam == phone; is_binary(UserParam) ->
    set_sip_param(user, UserParam, URI);
set_user_param(UserParam, _) ->
    error({user_param_expected, UserParam}).

%% @doc Clear user parameter from URI.
%% Raises error if URI is not SIP(S) URI.
-spec clear_user_param(uri()) -> uri().
clear_user_param(#uri{} = URI) ->
    clear_sip_param(user, URI).

%% @doc Get ttl parameter of URI.
%% Raises error if URI is not SIP(S) or TEL URI.
-spec ttl(uri()) -> ttl_param() | undefined.
ttl(#uri{data = #sip_uri_data{params = #{ttl := TTL}}}) ->
    TTL;
ttl(#uri{data = #sip_uri_data{}}) ->
    undefined;
ttl(#uri{data = #tel_uri_data{params = #{ttl := TTL}}}) ->
    TTL;
ttl(#uri{data = #tel_uri_data{}}) ->
    undefined;
ttl(#uri{} = URI) ->
    error({sip_uri_expected, URI}).

%% @doc Set ttl parameter of URI.
%% Raises error if URI is not SIP(S) URI.
-spec set_ttl(ttl_param(), uri()) -> uri().
set_ttl(TTL, #uri{} = URI) when TTL >= 0, TTL =< 255 ->
    set_sip_param(ttl, TTL, URI);
set_ttl(TTL, #uri{}) ->
    error({ttl_expected, TTL}).

%% @doc Clear TTL parameter from URI.
%% Raises error if URI is not SIP(S) URI.
-spec clear_ttl(uri()) -> uri().
clear_ttl(#uri{} = URI) ->
    clear_sip_param(ttl, URI).

%% @doc Get generic parameter of the URI.
%% Raises error if URI is not SIP(S) or TEL URI.
%% This function also can be used to get known parameters in generic form
%% Example:
%% ```
%%   <<"11">> = ersip_uri:gen_param(<<"ttl">>, ersip_uri:make(<<"sip:b;ttl=11">>)).
%%   true = ersip_uri:gen_param(<<"lr">>, ersip_uri:make(<<"sip:b;lr">>)).
%%   undefined = ersip_uri:gen_param(<<"lr">>, ersip_uri:make(<<"sip:b">>)).
%% '''
-spec gen_param(binary(), uri()) -> binary() | undefined.
gen_param(Name, #uri{data = #sip_uri_data{params = P}}) when is_binary(Name) ->
    case find_known_param(Name) of
        {ok, KnownParam} ->
            case P of
                #{KnownParam := V} ->
                    case raw_param({KnownParam, V}) of
                        {_, BinV} -> BinV;
                        _ -> true
                    end;
                _ -> undefined
            end;
        error ->
            case ersip_config:uri_param_to_lower() of
                false ->
                    maps:get(Name, P, undefined);
                true ->
                    maps:get(ersip_bin:to_lower(Name), P, undefined)
            end
    end;
gen_param(Name, #uri{data = #tel_uri_data{params = P}}) when is_binary(Name) ->
    case find_known_param(Name) of
        {ok, KnownParam} ->
            case P of
                #{KnownParam := V} ->
                    case raw_param({KnownParam, V}) of
                        {_, BinV} -> BinV;
                        _ -> true
                    end;
                _ -> undefined
            end;
        error ->
            case ersip_config:uri_param_to_lower() of
                false ->
                    maps:get(Name, P, undefined);
                true ->
                    maps:get(ersip_bin:to_lower(Name), P, undefined)
            end
    end;
gen_param(Name, #uri{} = URI) when is_binary(Name) ->
    error({sip_uri_expected, URI}).


%% @doc Set generic parameter of URI.
%% Raises error if URI is not SIP(S) or TEL URI.
-spec set_gen_param(binary(), binary(), uri()) -> uri().
set_gen_param(Name, Value, #uri{data = #sip_uri_data{} = D} = U)
  when is_binary(Name) andalso (is_binary(Value) orelse Value == true) ->
    ParamBin =
        case Value of
            true -> Name;
            _ -> <<Name/binary, "=", Value/binary>>
        end,
    case parse_and_add_param(ParamBin, D) of
        #sip_uri_data{} = NewD ->
            U#uri{data = NewD};
        {error, Reason} ->
            error({invalid_value, Reason})
    end;
set_gen_param(Name, Value, #uri{data = #tel_uri_data{} = D} = U)
    when is_binary(Name) andalso (is_binary(Value) orelse Value == true) ->
    ParamBin =
        case Value of
            true -> Name;
            _ -> <<Name/binary, "=", Value/binary>>
        end,
    case parse_and_add_param(ParamBin, D) of
        #tel_uri_data{} = NewD ->
            U#uri{data = NewD};
        {error, Reason} ->
            error({invalid_value, Reason})
    end;
set_gen_param(Name, Value, #uri{} = URI)
  when is_binary(Name) andalso (is_binary(Value) orelse Value == true) ->
    error({sip_uri_expected, URI}).

%% @doc Clear generic parameter of URI.
%% Raises error if URI is not SIP(S) or TEL URI.
-spec clear_gen_param(binary(), uri()) -> uri().
clear_gen_param(Name, #uri{data = #sip_uri_data{params = P} = D} = U) when is_binary(Name) ->
    case find_known_param(Name) of
        {ok, KnownParam} ->
            U#uri{data = D#sip_uri_data{params = maps:remove(KnownParam, P)}};
        error ->
            case ersip_config:uri_param_to_lower() of
                false ->
                    U#uri{data = D#sip_uri_data{params = maps:remove(Name, P)}};
                true ->
                    U#uri{data = D#sip_uri_data{params = maps:remove(ersip_bin:to_lower(Name), P)}}
            end
    end;
clear_gen_param(Name, #uri{data = #tel_uri_data{params = P} = D} = U) when is_binary(Name) ->
    case find_known_param(Name) of
        {ok, KnownParam} ->
            U#uri{data = D#tel_uri_data{params = maps:remove(KnownParam, P)}};
        error ->
            case ersip_config:uri_param_to_lower() of
                false ->
                    U#uri{data = D#tel_uri_data{params = maps:remove(Name, P)}};
                true ->
                    U#uri{data = D#tel_uri_data{params = maps:remove(ersip_bin:to_lower(Name), P)}}
            end
    end;
clear_gen_param(Name, #uri{} = URI) when is_binary(Name) ->
    error({sip_uri_expected, URI}).

%% @doc Set parameter of the URI
%% @deprecated
%% This function is deprecated. Please use set_gen_param for generic
%% form and set_transport, set_ttl, set_... for known params.
-spec set_param(uri_param_name(), term(), uri() | sip_uri_data() | tel_uri_data()) -> uri() | sip_uri_data() | tel_uri_data().
set_param(ParamName, Value, #uri{data = SIPData} = URI) ->
    URI#uri{data = set_param(ParamName, Value, SIPData)};
set_param(ParamName, Value, #sip_uri_data{params = P} = SIPData) ->
    SIPData#sip_uri_data{params = P#{ParamName => Value}};
set_param(ParamName, Value, #tel_uri_data{params = P} = SIPData) ->
    SIPData#tel_uri_data{params = P#{ParamName => Value}}.

%% @doc Get URI part by identify. This function is deprecated and will
%% be removed eventually.
%% @deprecated
-spec get(uri_part_name() | [uri_part_name()], uri()) -> uri_part() | [uri_part()].
get(Part, URI) when is_atom(Part) ->
    get_part(Part, URI);
get(Parts, URI) when is_list(Parts) ->
    lists:map(fun(Part) ->
                      get_part(Part, URI)
              end,
              Parts).

%% @doc Create URI from binary, raw representation and deprecated from URI parts.
%% Note that creation from URI parts are deprecated and will be
%% removed in future releases.
%% Raises error if URI cannot be constracted from this data (has invalid syntax).
%% Examples:
%% ```
%%   SIPURI = ersip_uri:make(<<"sip:a@b">>),
%%   SIPURI = ersip_uri:make(#{scheme => <<"sip">>, data => <<"a@b">>}),
%%   TelURI = ersip_uri:make(<<"tel:+16505550505">>),
%%   TelURI = ersip_uri:make(#{scheme => <<"tel">>, data => <<"+16505550505">>}).
%% '''
-spec make(binary() | raw() | [uri_part()]) -> uri().
make(Bin) when is_binary(Bin) ->
    case parse(Bin) of
        {ok, URI}       -> URI;
        {error, Reason} -> error(Reason)
    end;
make(#{scheme := Scheme, data := Data}) ->
    make(iolist_to_binary([Scheme, $:, Data]));
make(Parts) when is_list(Parts) ->
    Init =
        case proplists:get_value(scheme, Parts) of
            tel ->
                #uri{data = #tel_uri_data{}};
            _ ->
                #uri{data = #sip_uri_data{host = {ipv4, {0, 0, 0, 0}}}}
        end,
    lists:foldl(fun(Option, URI) ->
                        set_part(Option, URI)
                end,
                Init,
                Parts).

%% @doc Make URI comparable with =:= erlang operator.  This means that
%% if make_key(UriA) =:= make_key(UriB) then they equal by RFC3261 19.1.4 URI Comparison.
-spec make_key(uri()) -> uri().
make_key(#uri{} = URI) ->
    #uri{scheme = URI#uri.scheme,
         data = make_data_key(URI#uri.scheme, URI#uri.data)
        }.

%% @doc Parse URI from the binary
%% ```
%% SIP-URI          =  "sip:" [userinfo] hostport
%%                     uri-parameters [headers]
%% SIPS-URI         =  "sips:" [userinfo] hostport
%%                     uri-parameters [headers]
%% '''
-spec parse(binary()) -> parse_result().
parse(Binary) ->
    case split_scheme(Binary) of
        {<<>>, _} ->
            {error, {invalid_scheme, Binary}};
        {<<"sip">>, R} ->
            parse_uri(<<"sip">>, <<"sip">>, R);
        {<<"sips">>, R} ->
            parse_uri(<<"sips">>, <<"sips">>, R);
        {<<"tel">>, R} ->
            parse_uri(<<"tel">>, <<"tel">>, R);
        {S, R} ->
            parse_uri(ersip_bin:to_lower(S), S, R)
    end.

%% @doc Assemble URI to iolist.
-spec assemble(uri()) -> iolist().
assemble(#uri{scheme = Scheme, data = Data}) ->
    [assemble_scheme(Scheme), $:,
     assemble_data(Data)
    ].

%% @doc Assemble URI to binary.
-spec assemble_bin(uri()) -> binary().
assemble_bin(#uri{} = U) ->
    iolist_to_binary(assemble(U)).

%% @doc Returns true if URI is SIP or SIPS URI.
-spec is_sip(uri()) -> boolean().
is_sip(#uri{data = #sip_uri_data{}}) ->
    true;
is_sip(#uri{}) ->
    false.

%% @doc Get URI params.
%% @deprecated
-spec params(uri()) -> uri_params().
params(#uri{data = #sip_uri_data{params = Params}}) ->
    Params;
params(#uri{data = #tel_uri_data{params = Params}}) ->
    Params;
params(#uri{}) ->
    #{}.


%% @doc Get raw URI params as list.
-spec raw_params(uri()) -> [{binary(), binary()} | binary()].
raw_params(#uri{data = #sip_uri_data{params = Params}}) ->
    lists:map(fun raw_param/1, maps:to_list(Params));
raw_params(#uri{data = #tel_uri_data{params = Params}}) ->
    lists:map(fun raw_param/1, maps:to_list(Params)).

%% @doc Get raw URI headers as list.
-spec raw_headers(uri()) -> [{binary(), binary()}].
raw_headers(#uri{data = #sip_uri_data{headers = Headers}}) ->
    maps:to_list(Headers);
raw_headers(#uri{data = #tel_uri_data{headers = Headers}}) ->
    maps:to_list(Headers).

%% @doc Set raw URI headers from list.
-spec set_raw_headers([{binary(), binary()}], uri()) -> uri().
set_raw_headers(Headers, #uri{data = #sip_uri_data{} = Data} = URI) ->
    HMap = maps:from_list(Headers),
    URI#uri{data = Data#sip_uri_data{headers = HMap}};
set_raw_headers(Headers, #uri{data = #tel_uri_data{} = Data} = URI) ->
    HMap = maps:from_list(Headers),
    URI#uri{data = Data#tel_uri_data{headers = HMap}}.

%% @doc Clear all URI parameters.
-spec clear_params(uri()) -> uri().
clear_params(#uri{data = #sip_uri_data{} = SIPData} = URI) ->
    URI#uri{data = SIPData#sip_uri_data{params = #{}}};
clear_params(#uri{data = #tel_uri_data{} = SIPData} = URI) ->
    URI#uri{data = SIPData#tel_uri_data{params = #{}}};
clear_params(#uri{} = URI) ->
    URI.

%% @doc Set part of the URI
%% @deprecated
-spec set_part(uri_part(), uri()) -> uri().
set_part({scheme, _} = Scheme, #uri{} = URI) ->
    URI#uri{scheme = Scheme};
set_part({user, U} = User, #uri{data = #sip_uri_data{} = SIPData} = URI) when is_binary(U) ->
    URI#uri{data = SIPData#sip_uri_data{user = User}};
set_part({user, U} = User, #uri{data = #tel_uri_data{} = SIPData} = URI) when is_binary(U) ->
    URI#uri{data = SIPData#tel_uri_data{user = User}};
set_part({port, P}, #uri{data = #sip_uri_data{} = SIPData} = URI) when is_integer(P) ->
    URI#uri{data = SIPData#sip_uri_data{port = P}};
set_part({host, H}, #uri{data = #sip_uri_data{} = SIPData} = URI) ->
    case ersip_host:is_host(H) of
        true ->
            URI#uri{data = SIPData#sip_uri_data{host = H}};
        false ->
            error({invalid_host, H})
    end;
set_part(Part, _) ->
    error({invalid_part, Part}).

%% @doc Get part of the URI
%% @deprecated
-spec get_part(uri_part_name(), uri()) -> uri_part().
get_part(scheme, #uri{scheme = Scheme}) ->
    Scheme;
get_part(user, #uri{data = #sip_uri_data{user = User}}) ->
    User;
get_part(user, #uri{data = #tel_uri_data{user = User}}) ->
    User;
get_part(port, #uri{data = #sip_uri_data{port = Port}}) ->
    {port, Port};
get_part(host, #uri{data = #sip_uri_data{host = Host}}) ->
    {host, Host}.

%% @doc Clear not allowed par of the URI in context.
%% ```
%%                                                       dialog
%%                                           reg./redir. Contact/
%%               default  Req.-URI  To  From  Contact   R-R/Route  external
%% user          --          o      o    o       o          o         o
%% password      --          o      o    o       o          o         o
%% host          --          m      m    m       m          m         m
%% port          (1)         o      -    -       o          o         o
%% user-param    ip          o      o    o       o          o         o
%% method        INVITE      -      -    -       -          -         o
%% maddr-param   --          o      -    -       o          o         o
%% ttl-param     1           o      -    -       o          -         o
%% transp.-param (2)         o      -    -       o          o         o
%% lr-param      --          o      -    -       -          o         o
%% other-param   --          o      o    o       o          o         o
%% headers       --          -      -    -       o          -         o
%% '''
-spec clear_not_allowed_parts(Type, uri()) -> uri() when
      Type :: ruri
            | record_route.
clear_not_allowed_parts(ruri, #uri{data = #sip_uri_data{params = P} = SIPData} = URI) ->
    URI#uri{data = SIPData#sip_uri_data{
                     params = maps:without([<<"method">>], P),
                     headers = #{}
                    }
           };
clear_not_allowed_parts(ruri, #uri{data = #tel_uri_data{params = P} = SIPData} = URI) ->
    URI#uri{data = SIPData#tel_uri_data{
        params = maps:without([<<"method">>], P),
        headers = #{}
    }
    };
clear_not_allowed_parts(ruri, URI) ->
    %% For schemes other than sip/sips we do not clear anything
    URI;
clear_not_allowed_parts(record_route, #uri{data = #sip_uri_data{params = P} = SIPData} = URI) ->
    URI#uri{data = SIPData#sip_uri_data{
                     params = maps:without([<<"method">>, ttl], P),
                     headers = #{}
                    }};
clear_not_allowed_parts(record_route, URI) ->
    %% For schemes other than sip/sips we do not clear anything
    URI.

%% @doc Unquote and quote again headers.
-spec rebuild_header_values(uri()) -> uri().
rebuild_header_values(#uri{data = #sip_uri_data{headers = H} = D} = URI) ->
    NewH = maps:map(fun(_, V) -> rebuild_header_value(V) end, H),
    URI#uri{data = D#sip_uri_data{headers = NewH}};
rebuild_header_values(#uri{data = #tel_uri_data{headers = H} = D} = URI) ->
    NewH = maps:map(fun(_, V) -> rebuild_header_value(V) end, H),
    URI#uri{data = D#tel_uri_data{headers = NewH}};
rebuild_header_values(#uri{} = U) ->
    U.

%% @doc Get raw value (in plain erlang types) of the uri.
-spec raw(uri()) -> raw().
raw(#uri{} = URI) ->
    Base = #{scheme => scheme_bin(URI),
             data => data(URI)},
    enrich_raw(Base, URI).

%%===================================================================
%% Internal implementation
%%===================================================================

-spec parse_uri(Scheme :: binary(), OrigScheme :: binary(), URIData :: binary()) -> parse_result().
parse_uri(<<"sip">>, _, R) ->
    case parse_sipdata(R) of
        {ok, SipData} ->
            {ok, #uri{scheme = {scheme, sip}, data = SipData}};
        {error, Reason} ->
            {error, {invalid_sip_uri, Reason}}
    end;
parse_uri(<<"sips">>, _, R) ->
    case parse_sipdata(R) of
        {ok, SipData} ->
            {ok, #uri{scheme = {scheme, sips}, data = SipData}};
        {error, Reason} ->
            {error, {invalid_sip_uri, Reason}}
    end;
parse_uri(<<"tel">>, _, R) ->
    case parse_teldata(R) of
        {ok, TelData} ->
            {ok, #uri{scheme = {scheme, tel}, data = TelData}};
        {error, Reason} ->
            {error, {invalid_sip_uri, Reason}}
    end;
parse_uri(_, SchemeBin, R) ->
    case check_token(SchemeBin) of
        true ->
            URI = #uri{scheme = {scheme, SchemeBin}, data = #absolute_uri_data{opaque = R}},
            {ok, URI};
        false ->
            {error, {invalid_scheme, SchemeBin}}
    end.

-spec parse_sipdata(binary()) -> sip_uri_parse_result().
parse_sipdata(Bin) ->
    parse_usesrinfo(Bin).

-spec parse_teldata(binary()) -> tel_uri_parse_result().
parse_teldata(Bin) ->
    {User, Params, Headers} = split_uri(Bin),
    case patch_userinfo(User) of
        {ok, PatchedUserinfo} ->
            MaybeSIPData = {ok, #tel_uri_data{user = {user, PatchedUserinfo}}},
            MaybeSIPData1 = maybe_add_params(MaybeSIPData, Params),
            maybe_add_headers(MaybeSIPData1, Headers);
        {error, _} = Error ->
            Error
    end.

-spec parse_usesrinfo(binary()) -> sip_uri_parse_result().
parse_usesrinfo(Bin) ->
    case binary:split(Bin, <<"@">>) of
        [Userinfo, R] ->
            case patch_userinfo(Userinfo) of
                {ok, PatchedUserinfo} ->
                    parse_hostport({user, PatchedUserinfo}, R);
                {error, _} = Error ->
                    Error
            end;
        [R] ->
            parse_hostport(undefined, R)
    end.

%% hostport         =  host [":" port]
-spec parse_hostport({user, binary()} | undefined, binary()) -> sip_uri_parse_result().
parse_hostport(User, R) ->
    {HostPort, Params, Headers} = split_uri(R),
    MaybeSIPData =
        case split_hostport(HostPort) of
            {ok, {HostBin, <<>>}} ->
                case ersip_host:parse(HostBin) of
                    {ok, Host, <<>>} ->
                        {ok, #sip_uri_data{user = User, host = Host, host_orig = HostBin}};
                    {ok, _, End} ->
                        {error, {invalid_host, {garbage_at_the_end, End}}};
                    {error, Reason} ->
                        {error, {invalid_host, Reason}}
                end;
            {ok, {HostBin, PortBin}} ->
                case {ersip_host:parse(HostBin), parse_port(PortBin)} of
                    {{ok, Host, <<>>}, {ok, Port}} ->
                        {ok, #sip_uri_data{user = User, host = Host, port = Port, host_orig = HostBin}};
                    {{error, Reason}, _} ->
                        {error, {invalid_host, Reason}};
                    {_, {error, _} = Error} ->
                        Error
                end;
            {error, _} = Error ->
                Error
        end,
    MaybeSIPData1 = maybe_add_params(MaybeSIPData, Params),
    maybe_add_headers(MaybeSIPData1, Headers).

%% port           =  1*DIGIT
-spec parse_port(binary()) -> {ok, 0..65535} | {error, {invalid_port, binary()}}.
parse_port(Bin) ->
    case catch binary_to_integer(Bin) of
        Int when is_integer(Int) andalso Int >= 0 andalso Int =< 65535 ->
            {ok, Int};
        _ ->
            {error, {invalid_port, Bin}}
    end.

%% uri-parameters    =  *( ";" uri-parameter)
%% uri-parameter     =  transport-param / user-param / method-param
%%                      / ttl-param / maddr-param / lr-param / other-param
-spec maybe_add_params(sip_uri_parse_result() | tel_uri_parse_result(), binary()) -> sip_uri_parse_result() | tel_uri_parse_result().
maybe_add_params({error, _} = Err, _) ->
    Err;
maybe_add_params({ok, #sip_uri_data{} = SIPData}, <<>>) ->
    {ok, SIPData};
maybe_add_params({ok, #tel_uri_data{} = SIPData}, <<>>) ->
    {ok, SIPData};
maybe_add_params({ok, #sip_uri_data{} = SIPData}, ParamsBin) ->
    ParamsList = binary:split(ParamsBin, <<";">>, [global]),
    R =
        lists:foldl(fun(_, {error, _} = Err) ->
                            Err;
                       (Param, #sip_uri_data{} = SIPData1) ->
                            parse_and_add_param(Param, SIPData1)
                    end,
                    SIPData,
                    ParamsList),
    case R of
        #sip_uri_data{} ->
            {ok, R};
        {error, _} = Error ->
            Error
    end;
maybe_add_params({ok, #tel_uri_data{} = SIPData}, ParamsBin) ->
    ParamsList = binary:split(ParamsBin, <<";">>, [global]),
    R =
        lists:foldl(fun(_, {error, _} = Err) ->
            Err;
            (Param, #tel_uri_data{} = SIPData1) ->
                parse_and_add_param(Param, SIPData1)
                    end,
            SIPData,
            ParamsList),
    case R of
        #tel_uri_data{} ->
            {ok, R};
        {error, _} = Error ->
            Error
    end.

-spec maybe_add_headers(sip_uri_parse_result() | tel_uri_parse_result(), binary()) -> sip_uri_parse_result() | tel_uri_parse_result().
maybe_add_headers({error, _} = Err, _) ->
    Err;
maybe_add_headers({ok, #sip_uri_data{} = SIPData}, <<>>) ->
    {ok, SIPData};
maybe_add_headers({ok, #tel_uri_data{} = SIPData}, <<>>) ->
    {ok, SIPData};
maybe_add_headers({ok, #sip_uri_data{} = SIPData}, Headers) ->
    {ok, HeadersList, <<>>} = ersip_parser_aux:parse_kvps(fun uri_header_validator/2, <<"&">>, Headers),
    {ok, SIPData#sip_uri_data{headers = maps:from_list(HeadersList)}};
maybe_add_headers({ok, #tel_uri_data{} = SIPData}, Headers) ->
    {ok, HeadersList, <<>>} = ersip_parser_aux:parse_kvps(fun uri_header_validator/2, <<"&">>, Headers),
    {ok, SIPData#tel_uri_data{headers = maps:from_list(HeadersList)}}.

%% @private
%% @doc Parse and add parameters described in RFC3261
-spec parse_and_add_param(binary(), sip_uri_data() | tel_uri_data()) -> sip_uri_data() | tel_uri_data() | {error, term()}.
parse_and_add_param(Param, SIPData) ->
    Pair =
        case ersip_config:uri_param_to_lower() of
            false ->
                case binary:split(Param, <<"=">>) of
                    [Name] ->
                        {unquote_hex(Name), Name, <<>>};
                    [Name, Value] ->
                        {unquote_hex(Name), Name, Value}
                end;
            true ->
                case binary:split(Param, <<"=">>) of
                    [Name] ->
                        {ersip_bin:to_lower(unquote_hex(Name)), Name, <<>>};
                    [Name, Value] ->
                        {ersip_bin:to_lower(unquote_hex(Name)), Name, Value}
                end
        end,

    case Pair of
        {<<"transport">>, _, V} ->
            %% transport-param   =  "transport="
            %%                      ( "udp" / "tcp" / "sctp" / "tls"
            %%                      / other-transport)
            %% other-transport   =  token
            %%
            case ersip_transport:parse(V) of
                {error, _} = Err ->
                    Err;
                {ok, T} ->
                    set_param(transport, T, SIPData)
            end;

        {<<"maddr">>, _, A} ->
            %% maddr-param       =  "maddr=" host
            case ersip_host:parse(A) of
                {ok, Host, <<>>} ->
                    set_param(maddr, Host, SIPData);
                _ ->
                    {error, {invalid_maddr, A}}
            end;

        {<<"user">>, _, U} ->
            %%  user-param        =  "user=" ( "phone" / "ip" / other-user)
            case ersip_bin:to_lower(U) of
                <<"phone">> ->
                    set_param(user, phone, SIPData);
                <<"ip">> ->
                    set_param(user, ip, SIPData);
                _ ->
                    case check_token(U) of
                        true ->
                            set_param(user, U, SIPData);
                        false ->
                            {error, {einval, user_param}}
                    end
            end;

        {<<"lr">>, _, _} ->
            set_param(lr, true, SIPData);

        {<<"ttl">>, _, TTLBin} ->
            case catch binary_to_integer(TTLBin) of
                TTL when is_integer(TTL) andalso TTL >= 0 andalso TTL =< 255 ->
                    set_param(ttl, TTL, SIPData);
                _ ->
                    {error, {einval, ttl}}
            end;

        {Other, OrigName, OtherVal} ->
            case is_pname(OrigName) andalso is_pvalue(OtherVal) of
                true ->
                    set_param(Other, OtherVal, SIPData);
                false ->
                    {error, {invalid_parameter, Other}}
            end
    end.

%% userinfo         =  ( user / telephone-subscriber ) [":" password] "@"
%% password         =  *( unreserved / escaped / "&" / "=" / "+" / "$" / "," )
-spec patch_userinfo(binary()) -> {ok, binary()} | {error, term()}.
patch_userinfo(Bin) ->
    case binary:split(Bin, <<":">>) of
        [User, Password] ->
            case check_password(Password) of
                false -> {error, {bad_password, Password}};
                true ->
                    case patch_user(User, []) of
                        {error, _} = Error -> Error;
                        {ok, PUser} ->
                            {ok, <<PUser/binary, ":", Password/binary>>}
                    end
            end;
        [User] ->
            patch_user(User, [])
    end.

%% user             =  1*( unreserved / escaped / user-unreserved )
patch_user(<<>>, []) ->
    {error, empty_username};
patch_user(<<>>, Acc) ->
    {ok, iolist_to_binary(lists:reverse(Acc))};
patch_user(<<Char, R/binary>>, Acc) when ?is_unreserved(Char) orelse ?is_user_unreserved(Char) ->
    patch_user(R, [Char | Acc]);
patch_user(<<"%", A, B, R/binary>>, Acc) when ?is_HEXDIG(A) andalso ?is_HEXDIG(B) ->
    patch_user(R, [<<"%", A, B>> | Acc]);
patch_user(<<C, R/binary>>,  Acc) ->
    patch_user(R, [io_lib:format("%~2.16.0B", [C]) | Acc]).

-define(is_password_unreserved(X), (X =:= $& orelse X =:= $= orelse X =:= $+ orelse X =:= $$ orelse X =:= $,)).
%% password         =  *( unreserved / escaped / "&" / "=" / "+" / "$" / "," )
check_password(<<>>) ->
    true;
check_password(<<Char/utf8, R/binary>>) when ?is_unreserved(Char) orelse ?is_password_unreserved(Char) ->
    check_password(R);
check_password(<<"%", A/utf8, B/utf8, R/binary>>) when ?is_HEXDIG(A) andalso ?is_HEXDIG(B) ->
    check_password(R);
check_password(_) ->
    false.

check_token(Bin) ->
    ersip_parser_aux:check_token(Bin).


-spec make_data_key(scheme(), uri_data()) -> sip_uri_data() | tel_uri_data().
make_data_key({scheme, sip}, #sip_uri_data{} = SIPURIData) ->
    make_sip_data_key(SIPURIData);
make_data_key({scheme, sips}, #sip_uri_data{} = SIPURIData) ->
    make_sip_data_key(SIPURIData);
make_data_key({scheme, tel}, #tel_uri_data{} = SIPURIData) ->
    make_tel_data_key(SIPURIData);
make_data_key(Scheme, _) ->
    error({cannot_make_key, {unsupported_scheme, Scheme}}).

-spec make_sip_data_key(sip_uri_data()) -> sip_uri_data().
make_sip_data_key(#sip_uri_data{} = URIData) ->
    %%
    %% Comparison of the userinfo of SIP and SIPS URIs is case-
    %% sensitive.  This includes userinfo containing passwords or
    %% formatted as telephone-subscribers.  Comparison of all other
    %% components of the URI is case-insensitive unless explicitly
    %% defined otherwise.
    %%
    %% For two URIs to be equal, the user, password, host, and port
    %% components must match.
    #sip_uri_data{user  = userinfo_key(URIData#sip_uri_data.user),
                  host  = ersip_host:make_key(URIData#sip_uri_data.host),
                  host_orig = undefined,
                  port  = URIData#sip_uri_data.port,
                  params = params_key(URIData#sip_uri_data.params),
                  headers = headers_key(URIData#sip_uri_data.headers)
                 }.

-spec make_tel_data_key(tel_uri_data()) -> tel_uri_data().
make_tel_data_key(#tel_uri_data{} = URIData) ->
    #tel_uri_data{user = userinfo_key(URIData#tel_uri_data.user),
        params = params_key(URIData#tel_uri_data.params),
        headers = headers_key(URIData#tel_uri_data.headers)
    }.

%% @private
%% @doc URI userinfo part key
-spec userinfo_key(undefined | {user, binary()}) ->
                          undefined | {user, binary()}.
userinfo_key(undefined) ->
    undefined;
userinfo_key({user, Bin}) ->
    {user, ersip_bin:unquote_rfc_2396(Bin)}.

%% @private
%% @doc URI params key
-spec params_key(uri_params()) -> uri_params().
params_key(Params) ->
    maps:with([user, transport, ttl, method], Params).

%% @doc URI headers key
-spec headers_key(uri_headers()) -> uri_headers().
headers_key(Headers) ->
    maps:map(fun(Key, Value) ->
                     {Key, ersip_bin:unquote_rfc_2396(Value)}
             end,
             Headers).

-spec split_scheme(binary()) -> {binary(), binary()}.
split_scheme(Bin) ->
    case binary:split(Bin, <<":">>) of
        [Scheme, Suffix] ->
            {Scheme, Suffix};
        [Suffix] ->
            {<<>>, Suffix}
    end.

-spec split_uri(binary()) -> {HostPort, Params, Headers} when
      HostPort :: binary(),
      Params   :: binary(),
      Headers  :: binary().
split_uri(Bin) ->
    case binary:match(Bin, <<";">>) of
        nomatch ->
            {HostPort, Headers} = split_headers(Bin),
            {HostPort, <<>>, Headers};
        {_, 1} ->
            {HostPort, Rest} = split_params(Bin),
            {Params, Headers} = split_headers(Rest),
            {HostPort, Params, Headers}
    end.

-spec split_hostport(binary()) -> {ok, {binary(), binary()}} | {error, sip_uri_parse_error()}.
split_hostport(<<$[, _/binary>> = IPv6RefPort) ->
    case binary:match(IPv6RefPort, <<"]">>) of
        nomatch ->
            {error, {invalid_ipv6_reference, IPv6RefPort}};
        {Pos, 1} when Pos + 1 =:= byte_size(IPv6RefPort) ->
            %% No port specified
            {ok, {IPv6RefPort, <<>>}};
        {Pos, 1} ->
            Size = Pos+1,
            <<Host:Size/binary, Rest/binary>> = IPv6RefPort,
            case Rest of
                <<$:, Port/binary>> when Port =/= <<>> ->
                    {ok, {Host, Port}};
                Else ->
                    {error, {invalid_port, Else}}
            end
    end;
split_hostport(IPOrHost) ->
    case binary:split(IPOrHost, <<":">>) of
        [H, P] ->
            {ok, {H, P}};
        [H] ->
            {ok, {H, <<>>}}
    end.

-spec split_headers(binary()) -> {binary(), binary()}.
split_headers(Bin) ->
    case binary:split(Bin, <<"?">>) of
        [Prefix, Headers] ->
            {Prefix, Headers};
        [Prefix] ->
            {Prefix, <<>>}
    end.


-spec split_params(binary()) -> {binary(), binary()}.
split_params(Bin) ->
    case binary:split(Bin, <<";">>) of
        [Prefix, Headers] ->
            {Prefix, Headers}
    end.

-spec uri_header_validator(binary(), binary() | novalue) -> {ok, {binary(), binary()}}.
uri_header_validator(Key, novalue) ->
    {ok, {Key, <<>>}};
uri_header_validator(Key, Value) ->
    {ok, {Key, Value}}.


assemble_scheme({scheme, sip}) ->
    <<"sip">>;
assemble_scheme({scheme, sips}) ->
    <<"sips">>;
assemble_scheme({scheme, tel}) ->
    <<"tel">>;
assemble_scheme({scheme, Scheme}) ->
    Scheme.


-spec assemble_data(uri_data()) -> iolist().
assemble_data(#sip_uri_data{} = SIPData) ->
    [case SIPData#sip_uri_data.user of
         undefined ->
             [];
         User ->
             [assemble_user(User), $@]
     end,
     case SIPData#sip_uri_data.host_orig of
         undefined ->
             ersip_host:assemble(SIPData#sip_uri_data.host);
         Val when is_binary(Val) ->
             Val
     end,
     case SIPData#sip_uri_data.port of
         undefined ->
             [];
         Port ->
             [$:, integer_to_binary(Port)]
     end,
     assemble_params(SIPData#sip_uri_data.params),
     assemble_headers(SIPData#sip_uri_data.headers)
    ];
assemble_data(#tel_uri_data{} = SIPData) ->
    [case SIPData#tel_uri_data.user of
         undefined ->
             [];
         User ->
             [assemble_user(User)]
     end,
        assemble_params(SIPData#tel_uri_data.params),
        assemble_headers(SIPData#tel_uri_data.headers)
    ];
assemble_data(#absolute_uri_data{opaque = Data}) ->
    Data.

-spec assemble_user({user, binary()}) -> binary().
assemble_user({user, UserBin}) ->
    UserBin.

-spec assemble_params(uri_params()) -> [iolist()].
assemble_params(Params) ->
    lists:map(fun assemble_param/1,
              maps:to_list(Params)).

-spec assemble_headers(uri_headers()) -> [iolist()].
assemble_headers(Headers) ->
    case Headers == #{} of
        true ->
            [];
        false ->
            [$?,
             ersip_iolist:join(
               <<"&">>,
               lists:map(fun ({Name, Value}) ->
                                 [Name, $=, Value]
                         end,
                         maps:to_list(Headers)))
            ]
    end.

-spec set_sip_param(known_param(), known_param_value(), uri()) -> uri().
set_sip_param(Name, Value, #uri{data = #sip_uri_data{params = P} = D} = U) ->
    U#uri{data = D#sip_uri_data{params = P#{Name => Value}}};
set_sip_param(Name, Value, #uri{data = #tel_uri_data{params = P} = D} = U) ->
    U#uri{data = D#tel_uri_data{params = P#{Name => Value}}};
set_sip_param(_, _, #uri{} = URI) ->
    error({sip_uri_expected, URI}).

-spec clear_sip_param(known_param(), uri()) -> uri().
clear_sip_param(Name, #uri{data = #sip_uri_data{params = P} = SIPData} = URI) ->
    URI#uri{data = SIPData#sip_uri_data{params = maps:remove(Name, P)}};
clear_sip_param(Name, #uri{data = #tel_uri_data{params = P} = SIPData} = URI) ->
    URI#uri{data = SIPData#tel_uri_data{params = maps:remove(Name, P)}};
clear_sip_param(_, #uri{} = URI) ->
    error({sip_uri_expected, URI}).


-spec raw_param({uri_param_name(), term()}) -> {binary(), binary()} | binary().
raw_param({transport, Value}) -> {<<"transport">>, ersip_transport:assemble_bin(Value)};
raw_param({maddr, Host})      -> {<<"maddr">>,    ersip_host:assemble_bin(Host)};
raw_param({lr, _})            -> <<"lr">>;
raw_param({user, ip})         -> {<<"user">>, <<"ip">>};
raw_param({user, phone})      -> {<<"user">>, <<"phone">>};
raw_param({user, Bin})
  when is_binary(Bin)         -> {<<"user">>, Bin};
raw_param({ttl, TTL})         -> {<<"ttl">>, integer_to_binary(TTL)};
raw_param({Name, <<>>})
  when is_binary(Name)        -> Name;
raw_param({Name, Value})      -> {Name, Value}.

-spec find_known_param(binary()) -> {ok, known_param()} | error.
find_known_param(<<"ttl">>)       -> {ok, ttl};
find_known_param(<<"maddr">>)     -> {ok, maddr};
find_known_param(<<"user">>)      -> {ok, user};
find_known_param(<<"lr">>)        -> {ok, lr};
find_known_param(<<"transport">>) -> {ok, transport};
find_known_param(_)               -> error.


-spec assemble_param({Name, Value}) -> iolist() when
      Name :: uri_param_name(),
      Value :: term().
assemble_param(Pair) ->
    case raw_param(Pair) of
        {Name, Val} -> [<<";">>, Name, <<"=">>, Val];
        Name -> [<<";">>, Name]
    end.

-spec is_pname(binary()) -> boolean().
is_pname(<<>>) ->
    false;
is_pname(Val) ->
    is_paramchar_string(Val).

-spec is_pvalue(binary()) -> boolean().
is_pvalue(<<>>) ->
    true;
is_pvalue(Val) ->
    is_paramchar_string(Val).

%% paramchar         =  param-unreserved / unreserved / escaped
%% param-unreserved  =  "[" / "]" / "/" / ":" / "&" / "+" / "$"
-define(is_param_unreserved(X),  (X == $[ orelse X == $]
                          orelse X == $/ orelse X == $:
                          orelse X == $& orelse X == $+
                          orelse X == $$)).


-spec is_paramchar_string(binary()) -> boolean().
is_paramchar_string(<<>>) ->
    true;
is_paramchar_string(<<C:8, Rest/binary>>) when ?is_unreserved(C)
                                        orelse ?is_param_unreserved(C) ->
    is_paramchar_string(Rest);
is_paramchar_string(<<$%, H1:8, H2:8, Rest/binary>>) when ?is_HEXDIG(H1) andalso ?is_HEXDIG(H2) ->
    %% escaped     =  "%" HEXDIG HEXDIG
    is_paramchar_string(Rest);
is_paramchar_string(_) ->
    false.


-spec unquote_hex(binary()) -> binary().
unquote_hex(Bin) ->
    do_unquote_hex(Bin, Bin, {0, 0}, []).

-spec do_unquote_hex(binary(), binary(), {non_neg_integer(), integer()}, iolist()) -> binary().
do_unquote_hex(<<>>, Orig, {_, Len}, []) when Len == byte_size(Orig) ->
    Orig;
do_unquote_hex(<<>>, _, {_, 0}, Acc) ->
    iolist_to_binary(lists:reverse(Acc));
do_unquote_hex(<<>>, Orig, Part, Acc) ->
    PartBin = binary:part(Orig, Part),
    iolist_to_binary(lists:reverse([PartBin | Acc]));
do_unquote_hex(<<$%, H1:8, H2:8, Rest/binary>>, Orig, {Pos, Len} = Part, Acc) when ?is_HEXDIG(H1) andalso ?is_HEXDIG(H2) ->
    Char = 16 * hex_char_to_num(H1) + hex_char_to_num(H2),
    case Len of
        0 ->
            do_unquote_hex(Rest, Orig, {Pos + 3, 0}, [Char | Acc]);
        _ ->
            PartBin = binary:part(Orig, Part),
            do_unquote_hex(Rest, Orig, {Pos + Len + 3, 0}, [Char, PartBin | Acc])
    end;
do_unquote_hex(<<_:8, Rest/binary>>, Orig, {Pos, Len}, Acc) ->
    do_unquote_hex(Rest, Orig, {Pos, Len+1}, Acc).

-spec hex_char_to_num(char()) -> 0..15.
hex_char_to_num(X) when X >= $0 andalso X =< $9 ->
    X - $0;
hex_char_to_num(X) when X >= $A andalso X =< $F ->
    X - $A + 10;
hex_char_to_num(X) when X >= $a andalso X =< $f ->
    X - $a + 10.

-spec rebuild_header_value(binary()) -> binary().
rebuild_header_value(Value) ->
    Bytes = binary_to_list(unquote_hex(Value)),
    Escaped = [escape_header_byte(B) || B <- Bytes],
    iolist_to_binary(Escaped).


%% hvalue          =  *( hnv-unreserved / unreserved / escaped )
%% hnv-unreserved  =  "[" / "]" / "/" / "?" / ":" / "+" / "$"
-spec escape_header_byte(char()) -> char() | string().
escape_header_byte(V) when ?is_unreserved(V);
                           ?is_hnv_unreserved(V) ->
    V;
escape_header_byte(V) ->
    io_lib:format("%~2.16.0B", [V]).

-spec enrich_raw(raw(), uri()) -> raw().
enrich_raw(Base, #uri{data = #sip_uri_data{}} = URI) ->
    RawList =
      [case P of
           {K, V} -> {ersip_bin:to_lower(K), V};
           K -> {ersip_bin:to_lower(K), <<>>}
       end || P <- raw_params(URI)],
    Parts = [{user, user(URI)},
             {host, ersip_host:raw(host(URI))},
             {port, port(URI)},
             {params, maps:from_list(RawList)},
             {headers, raw_headers(URI)}],
    NonEmpty = [{K, V} || {K, V} <- Parts, V /= undefined andalso V /= [] andalso V /= #{}],
    Base#{sip => maps:from_list(NonEmpty)};
enrich_raw(Base, #uri{data = #tel_uri_data{}} = URI) ->
    RawList =
        [case P of
             {K, V} -> {ersip_bin:to_lower(K), V};
             K -> {ersip_bin:to_lower(K), <<>>}
         end || P <- raw_params(URI)],
    Parts = [{user, user(URI)},
        {params, maps:from_list(RawList)},
        {headers, raw_headers(URI)}],
    NonEmpty = [{K, V} || {K, V} <- Parts, V /= undefined andalso V /= [] andalso V /= #{}],
    Base#{sip => maps:from_list(NonEmpty)};
enrich_raw(Base, #uri{}) ->
    Base.
