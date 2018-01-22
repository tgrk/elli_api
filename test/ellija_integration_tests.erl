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
        ?assertMatch({ok, _}, application:ensure_all_started(?APP)),
        ?assertMatch({ok, _Pid}, ellija:start()),
        ok
      end,
      fun(_) ->
        ?assertEqual(ok, ellija:stop()),
        application:stop(?APP)
      end,
      [
          {"Default endpoint",      fun test_default_endpoint/0}
        , {"Basic routing",         fun test_basic_routing/0}
        , {"Simple CRUD",           fun test_simple_crud/0}
        , {"JSONAPI sparse fields", fun test_jsonapi_query_args/0}
      ]
  }.

%% =============================================================================

test_default_endpoint() ->
  RequestFun = fun() -> http_request("http://localhost:8089/") end,
  assert_payload(
    200, #{<<"message">> => <<"Hello, World!">>},
    RequestFun()
  ).

test_basic_routing() ->
  Data = [
    #{<<"id">> => 1, <<"title">> => <<"Hey!">>}
  ],
  Config = #{routes => [
      {get, <<"/articles">>, fun (_Req, _) ->
        Payload = eja_response:serialize(<<"articles">>, Data, #{}),
        ellija_resp:ok({ok, Payload})
      end, []}
    , {get, <<"/articles/:id">>, fun (_Req, _) ->
        Payload = eja_response:serialize(<<"articles">>, hd(Data), #{}),
        ellija_resp:ok({ok, Payload})
      end, []}
  ]},
  ?assertEqual(ok, ellija_config:set(Config)),

  ?assert(length(ellija_config:get(routes)) == 2),

  RequestFun = fun(Path) -> http_request("http://localhost:8089/" ++ Path) end,

  assert_payload(
    200, Data, RequestFun("articles")
  ),
  assert_payload(
    200, hd(Data), RequestFun("articles/1")
  ),
  assert_payload(
    404, <<"Not Found">>, RequestFun("foo")
  ),

  ok.

test_simple_crud() ->
  Config = #{
    routes => [
        {get,    <<"/articles">>,     fun (_, _) -> ellija_resp:ok({ok, <<"articles">>}) end, []}
      , {post,   <<"/articles">>,     fun (_, _) -> ellija_resp:created() end, []}
      , {get,    <<"/articles/:id">>, fun (_, _) -> ellija_resp:ok({ok, <<"detail:1">>}) end, []}
      , {put,    <<"/articles/:id">>, fun (_, _) -> ellija_resp:ok({ok, <<"updated:1">>}) end, []}
      , {delete, <<"/articles/:id">>, fun (_, _) -> ellija_resp:no_content() end, []}
  ]},
  ?assertEqual(ok, ellija_config:set(Config)),

  ?assert(length(ellija_config:get(routes)) == 5),

  RequestFun = fun (get, Path) ->
                      http_request("http://localhost:8089/" ++ Path);
                    (Verb, Path) ->
                      http_request(Verb, "http://localhost:8089/" ++ Path, [])
                end,

  ?assertMatch(
    {ok, {200, _, <<"articles">>}},
    RequestFun(get, "articles")
  ),
  ?assertMatch(
    {ok, {201, _, _}},
    RequestFun(post, "articles")
  ),

  ?assertMatch(
    {ok, {200, _, <<"detail:1">>}},
    RequestFun(get, "articles/1")
  ),

  ?assertMatch(
    {ok, {200, _, <<"updated:1">>}},
    RequestFun(put, "articles/1")
  ),

  ?assertMatch(
    {ok, {204, _, []}},
    RequestFun(delete, "articles/1")
  ),

  ok.

test_jsonapi_query_args() ->
  Config = #{
    routes => [
      {get, <<"/articles">>, fun (Req, _Params) ->
        Data = [],
        Args = ellija_req:parse_args(Req),
        ellija_resp:ok(
          eja:create(<<"articles">>, Data, Args)
        )
      end, []}
  ]},
  ?assertEqual(ok, ellija_config:set(Config)),

  Url = "articles?include=author&fields[articles]=title,body,author&fields[people]=name",
  RequestFun = fun(Path) -> http_request("http://localhost:8089/" ++ Path) end,

  R = RequestFun(Url),

  % ?assertMatch(
  %   {ok, {200, _, #{<<"data">> => _}}},
  %   RequestFun(Url)
  % )

  ?debugFmt("result=~p", [R]),

  ok.

%% =============================================================================

http_request(Url) ->
  http_request(get, Url).

http_request(get, Url) ->
  handle_http_response(
    httpc:request(get, {Url, get_jsonapi_headers()}, [{timeout, infinity}], [])
  ).

http_request(Method, Url, Payload) ->
  Headers = [hd(get_jsonapi_headers())],
  Request = {Url, Headers, "application/vnd.api+json", Payload},
  handle_http_response(
    httpc:request(Method, Request, [{timeout, infinity}], [])
  ).

handle_http_response({ok, {{_, Status, _}, Headers, []}}) ->
  {ok, {Status, Headers, []}};
handle_http_response({ok, {{_, Status, _}, Headers, Response}}) ->
  {ok, {Status, Headers, jiffy:decode(Response, [return_maps])}};
handle_http_response({error, _Reason} = Error) ->
  Error.

get_jsonapi_headers() ->
  [eja:get_header(content_type), eja:get_header(accept)].

assert_payload(ExpectedStatus, ExpectedPayload, {ok, {Status, _, Payload}}) ->
  ?assertEqual(ExpectedStatus, Status),
  case is_map(Payload) andalso maps:is_key(<<"data">>, Payload) of
    true ->
      ?assertEqual(
        eja_response:serialize(<<"articles">>, ExpectedPayload, #{}),
        Payload
      );
    false ->
      ?assertEqual(ExpectedPayload, Payload)
  end,
  ok;
assert_payload(_ExpectedStatus, ExpectedReason, {error, Reason}) ->
  ?assertEqual(ExpectedReason, Reason),
  ok;
assert_payload(_ExpectedStatus, _ExpectedReason, _Result) ->
  ?assert(false).