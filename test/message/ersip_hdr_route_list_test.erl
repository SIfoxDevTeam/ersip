%%%
%%% Copyright (c) 2018, 2020 Dmitry Poroh
%%% All rights reserved.
%%% Distributed under the terms of the MIT License. See the LICENSE file.
%%%
%%% Route headers list test
%%%

-module(ersip_hdr_route_list_test).

-include_lib("eunit/include/eunit.hrl").

%%%===================================================================
%%% Cases
%%%===================================================================

topmost_route_test() ->
    Route = topmost_route(<<"<sip:alice@atlanta.com>,<sip:carol@chicago.com>,<sip:bob@biloxi.com>">>),
    ?assertEqual(ersip_uri:make(<<"sip:alice@atlanta.com">>), ersip_hdr_route:uri(Route)),
    RouteWithExt = topmost_route(<<"<sip:alice@atlanta.com>;extension=1">>),
    ?assertEqual(ersip_uri:make(<<"sip:alice@atlanta.com">>), ersip_hdr_route:uri(RouteWithExt)),
    ?assertEqual([{<<"extension">>, <<"1">>}], ersip_hdr_route:params(RouteWithExt)),
    RouteWithLR = topmost_route(<<"<sip:alice@atlanta.com;lr>">>),
    URIWithLR = ersip_hdr_route:uri(RouteWithLR),
    ?assertEqual(#{lr => true}, ersip_uri:params(URIWithLR)),
    ok.

make_route_test() ->
    Route = ersip_hdr_route_list:make(<<"<sip:alice@atlanta.com>,<sip:carol@chicago.com>,<sip:bob@biloxi.com>">>),
    ?assertEqual(ersip_uri:make(<<"sip:alice@atlanta.com">>), ersip_hdr_route:uri(ersip_route_set:first(Route))),
    ?assertError({error, _}, ersip_hdr_route_list:make(<<"?">>)),
    ?assertError({error, _}, ersip_hdr_route_list:make(<<"<sip:alice@atlanta.com>,?">>)),
    ok.

parse_fail_in_middle_test() ->
    H1 = ersip_hdr:new(<<"Route">>),
    H2 = ersip_hdr:add_value(<<"<sip:alice@atlanta.com>;x&=x">>, H1),
    H3 = ersip_hdr:add_value(<<"<sip:bob@biloxi.com>">>, H2),
    ?assertMatch({error, _}, ersip_hdr_route_list:parse(H3)),
    ok.

bad_topmost_test() ->
    H = ersip_hdr:new(<<"Route">>),
    H1 = ersip_hdr:add_value(<<"?">>, H),
    ?assertMatch({error, _}, ersip_hdr_route_list:parse(H1)).

bad_middle_test() ->
    H = ersip_hdr:new(<<"Route">>),
    H1 = ersip_hdr:add_value(<<"<sip:alice@atlanta.com>,?,<sip:bob@biloxi.com>">>, H),
    ?assertMatch({error, _}, ersip_hdr_route_list:parse(H1)).

route_with_inval_lr_test() ->
    Route = topmost_route(<<"<sip:alice@atlanta.com>,<sip:carol@chicago.com>,<sip:bob@biloxi.com>">>),
    ?assertEqual(ersip_uri:make(<<"sip:alice@atlanta.com">>), ersip_hdr_route:uri(Route)),
    RouteWithExt = topmost_route(<<"<sip:alice@atlanta.com>;extension=1">>),
    ?assertEqual(ersip_uri:make(<<"sip:alice@atlanta.com">>), ersip_hdr_route:uri(RouteWithExt)),
    ?assertEqual([{<<"extension">>, <<"1">>}], ersip_hdr_route:params(RouteWithExt)),
    RouteWithInvalLR = topmost_route(<<"<sip:alice@atlanta.com>;lr">>),
    URIWithInvalLR = ersip_hdr_route:uri(RouteWithInvalLR),
    ?assertEqual(#{}, ersip_uri:params(URIWithInvalLR)),
    ok.

invalid_rr_param_test() ->
    H = ersip_hdr:new(<<"Route">>),
    H1 = ersip_hdr:add_value(<<"<sip:alice@atlanta.com>;x&">>, H),
    ?assertMatch({error, _}, ersip_hdr_route_list:parse(H1)),
    H2 = ersip_hdr:new(<<"Route">>),
    H3 = ersip_hdr:add_value(<<"<sip:alice@atlanta.com>;x&=x">>, H2),
    ?assertMatch({error, _}, ersip_hdr_route_list:parse(H3)).

empty_route_test() ->
    H = ersip_hdr:new(<<"Route">>),
    Empty = ersip_route_set:new(),
    ?assertEqual(true, ersip_route_set:is_empty(Empty)),
    ?assertEqual({ok,  Empty}, ersip_hdr_route_list:parse(H)).

rebuild_route_test() ->
    rebuild(<<"<sip:alice@atlanta.com>,<sip:carol@chicago.com>,<sip:bob@biloxi.com>">>),
    rebuild(<<"<sip:alice@atlanta.com>;test=1,<sip:carol@chicago.com>,<sip:bob@biloxi.com>">>),
    rebuild(<<"<sip:alice@atlanta.com;lr>">>),
    rebuild(<<"<sip:alice@atlanta.com>">>).

%%%===================================================================
%%% Helpers
%%%===================================================================

create(RouteBin) ->
    HRoute = ersip_hdr:new(<<"Route">>),
    ersip_hdr:add_value(RouteBin, HRoute).

topmost_route(RouteBin) ->
    HRoute = create(RouteBin),
    case ersip_hdr_route_list:parse(HRoute) of
        {ok, RouteSet} -> ersip_route_set:first(RouteSet);
        Error ->
            error(Error)
    end.

rebuild(Bin) ->
    Route = ersip_hdr_route_list:make(Bin),
    {ok, Route1} = ersip_hdr_route_list:parse(ersip_hdr_route_list:build(<<"Route">>, Route)),
    ?assertEqual(Route, Route1).
