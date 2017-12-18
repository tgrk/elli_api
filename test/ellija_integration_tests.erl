%%%----------------------------------------------------------------------------
%%% @doc
%%% Main API integration test
%%% @end
%%%----------------------------------------------------------------------------
-module(ellija_integration_tests).

-export([ellija_integration_test_/0]).

-include("ellija.hrl").
-include_lib("eunit/include/eunit.hrl").

%% =============================================================================

ellija_integration_test_() ->
    {setup,
        fun() ->
            ?assertMatch({ok, _}, application:ensure_all_started(?APP))
        end,
        fun(_) ->
             application:stop(?APP)
        end,
        [
              {"Default endpoint", fun test_default_endpoint/0}
            , {"Basic routing",    fun test_basic_routing/0}
            , {"Simple CRUD",      fun test_simple_crud/0}
        ]
    }.

%% =============================================================================

test_default_endpoint() ->
    RequestFun = fun() -> http_request("http://localhost:8089/") end,

    ?assertMatch({error, {failed_connect, _}}, RequestFun()),

    assert_server_start(),
    ?assertMatch({ok, {200, _, "Hello, World!"}}, RequestFun()),
    assert_server_stop(),
    ok.

test_basic_routing() ->
    Config = #{routes => [
          {get, <<"/list">>, fun (_Req, _) -> ellija_resp:ok({ok, <<"list">>}) end, []}
        , {get, <<"/list/:id">>, fun (_Req, _) -> ellija_resp:ok({ok, <<"detail:1">>}) end, []}
    ]},
    assert_server_start(Config),

    ?assert(length(ellija_config:get(routes)) == 2),

    RequestFun = fun(Path) -> http_request("http://localhost:8089/" ++ Path) end,
    ?assertMatch({ok, {200, _, "list"}}, RequestFun("list")),
    ?assertMatch({ok, {200, _, "detail:1"}}, RequestFun("list/1")),
    ?assertMatch({ok, {404, _, "Not Found"}}, RequestFun("foo")),

    assert_server_stop(),
    ok.

test_simple_crud() ->
    Config = #{routes => [
          {get,    <<"/list">>,     fun (_, _) -> ellija_resp:ok({ok, <<"list">>}) end, []}
        , {post,   <<"/list">>,     fun (_, _) -> ellija_resp:created() end, []}
        , {get,    <<"/list/:id">>, fun (_, _) -> ellija_resp:ok({ok, <<"detail:1">>}) end, []}
        , {put,    <<"/list/:id">>, fun (_, _) -> ellija_resp:ok({ok, <<"updated:1">>}) end, []}
        , {delete, <<"/list/:id">>, fun (_, _) -> ellija_resp:no_content() end, []}

    ]},
    assert_server_start(Config),

    ?assert(length(ellija_config:get(routes)) == 5),

    RequestFun = fun(Verb, Path) -> http_request(Verb, "http://localhost:8089/" ++ Path, []) end,

    ?assertMatch({ok, {200, _, "list"}},      RequestFun(get,    "list")),
    ?assertMatch({ok, {201, _, _}},           RequestFun(post,   "list")),

    ?assertMatch({ok, {200, _, "detail:1"}},  RequestFun(get,    "list/1")),

    ?assertMatch({ok, {200, _, "updated:1"}}, RequestFun(put,    "list/1")),


    ?assertMatch({ok, {204, _, ""}},          RequestFun(delete, "list/1")),

    assert_server_stop(),
    ok.

%% =============================================================================

http_request(Url) ->
    handle_http_response(httpc:request(Url)).


http_request(get, Url, []) ->
    http_request(Url);
http_request(Method, Url, Payload) ->
    Request = {Url, [], "application/json", Payload},
    Result = httpc:request(Method, Request, [{timeout, infinity}], []),
    handle_http_response(Result).

handle_http_response({ok, {{_, Status, _}, Headers, Response}}) ->
    {ok, {Status, Headers, Response}};
handle_http_response({error, _Reason} = Error) ->
    Error.

assert_server_start() ->
    ?assertMatch({ok, _Pid}, ellija:start()).

assert_server_start(Config) ->
    ?assertMatch({ok, _Pid}, ellija:start(Config)).

assert_server_stop() ->
    ?assertEqual(ok, ellija:stop()).