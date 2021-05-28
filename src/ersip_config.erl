-module(ersip_config).

%% API
-export([init/0, uri_param_to_lower/0, terminate_dialog_408_481/0]).

init() ->
    persistent_term:put(ersip_uri_param_to_lower, application:get_env(ersip, uri_param_to_lower, true)),
    persistent_term:put(ersip_terminate_dialog_408_481, application:get_env(ersip, ersip_terminate_dialog_408_481, true)).

uri_param_to_lower() ->
    persistent_term:get(ersip_uri_param_to_lower, true).

terminate_dialog_408_481() ->
    persistent_term:get(ersip_terminate_dialog_408_481, true).
