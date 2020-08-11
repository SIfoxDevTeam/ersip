-module(ersip_config).

%% API
-export([init/0, uri_param_to_lower/0]).

init() ->
    persistent_term:put(ersip_uri_param_to_lower, application:get_env(ersip, uri_param_to_lower, true)).

uri_param_to_lower() ->
    persistent_term:get(ersip_uri_param_to_lower, true).
