%%%----------------------------------------------------------------------------
%%% @doc
%%%
%%% @end
%%%----------------------------------------------------------------------------
-module(ellija_api).

-behaviour(elli_handler).

-include("ellija.hrl").
-include_lib("kernel/include/file.hrl").

-ifdef(TEST).
-include_lib("eunit/include/eunit.hrl").
-endif.

%% API
-export([ handle/2
        , handle_event/3
        ]).

%%%============================================================================
%%% API functions
%%%============================================================================

handle(Req, _Args) ->
  try
    handle(elli_request:method(Req), elli_request:path(Req), Req)
  catch
    Class:Reason ->
      error_logger:error_msg("Unhandled REST API ~p: ~p: ~p",
          [Class, Reason, erlang:get_stacktrace()]),
      ellija_resp:internal_error()
  end.

handle_event(_, _, _) ->
  ok.

%%%============================================================================
%%% Internal functions
%%%============================================================================

handle(Method, Path, Req) ->
  Verb    = convert_method(Method),
  Headers = ellija_config:get(headers),
  Routes  = ellija_config:get(routes),

  case match_handler(Routes, Verb, Path) of
    {ok, Handler, Params} ->
      Handler(Req, Params);
    {error, not_found} ->
      io:format("Not found=~p~n", [Path]),
      ellija_resp:not_found(Headers)
  end.

match_handler([], _Verb, _ReqPath) ->
  {error, not_found};
match_handler([{Verb, P, H, _M} | T], Verb, ReqPath) ->
  HandlerPath = split_path(P),
    case match_path(HandlerPath, ReqPath, #{}) of
      {true, Params} ->
        {ok, H, Params};
      {false, _EmptyParams} ->
        match_handler(T, Verb, ReqPath)
    end;
match_handler([_H | T], Verb, ReqPath) ->
  match_handler(T, Verb, ReqPath).

match_path([], [], Acc) ->
  {true, Acc};
match_path([<<":", (H1)/binary>> | T1], [H2 | T2], Acc) ->
  match_path(T1, T2, maps:put(H1, H2, Acc));
match_path([H1 | T1], [H1 | T2], Acc) ->
  match_path(T1, T2, Acc);
match_path(_, _, _Acc) ->
  {false, #{}}.

split_path(<<"/">>) -> [];
split_path(Path) ->
  binary:split(Path, [<<"/">>], [global, trim_all]).

convert_method('GET')     -> get;
convert_method('POST')    -> post;
convert_method('PUT')     -> put;
convert_method('DELETE')  -> delete;
convert_method('TRACE')   -> head;
convert_method('HEAD')    -> trace;
convert_method('OPTIONS') -> options.

%%%============================================================================
%%% Tests
%%%============================================================================

-ifdef(TEST).

match_handler_test() ->
  ExtractHandlerFun = fun ({_V, _P, H, _M}) -> H end,

  Route1 = {get,    <<"/">>,         fun (_, _) -> get end, []},
  Route2 = {get,    <<"/list/:id">>, fun (_, _) -> get end, []},
  Route3 = {post,   <<"/list">>,     fun (_, _) -> post end, []},
  Route4 = {delete, <<"/list/:id">>, fun (_, _) -> delete end, []},
  Routes = [Route1, Route2, Route3, Route4],

  ?assertEqual(
    {ok, ExtractHandlerFun(Route1), #{}},
    match_handler(Routes, get, split_path(<<"/">>))
  ),
  ?assertEqual(
    {ok, ExtractHandlerFun(Route2), #{<<"id">> => <<"1">>}},
    match_handler(Routes, get, split_path(<<"/list/1">>))
  ),
  ?assertEqual(
    {ok, ExtractHandlerFun(Route3), #{}},
    match_handler(Routes, post, split_path(<<"/list">>))
  ),
  ?assertEqual(
    {ok, ExtractHandlerFun(Route4), #{<<"id">> => <<"2">>}},
    match_handler(Routes, delete, split_path(<<"/list/2">>))
  ),

  ok.

match_path_test() ->
  Cases = [
          {[], [], {true, #{}}}
      ,   {[<<"/">>, <<"foo">>], [], {false, #{}}}
      ,   {
            [<<"/">>, <<"foo">>, <<"/">>, <<":id">>]
          , [<<"/">>, <<"foo">>, <<"/">>, <<"1">>]
          , {true, #{<<"id">> => <<"1">>}}
          }
      ,  {
            [<<"/">>, <<"foo">>, <<"/">>, <<":id">>, <<"/">>, <<":user_id">>]
          , [<<"/">>, <<"foo">>, <<"/">>, <<"1">>, <<"/">>, <<"2">>]
          , {true, #{<<"id">> => <<"1">>, <<"user_id">> => <<"2">>}}
      }
  ],

  lists:foreach(fun ({Path1, Path2, Result}) ->
    ?assertEqual(Result, match_path(Path1, Path2, #{}))
  end, Cases),

  ok.

-endif.