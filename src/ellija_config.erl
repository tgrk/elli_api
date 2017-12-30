%%%----------------------------------------------------------------------------
%%% @doc
%%% Holds server configuration
%%% @end
%%%----------------------------------------------------------------------------

-module(ellija_config).

-behaviour(gen_server).

-include("ellija.hrl").

-ifdef(TEST).
-include_lib("eunit/include/eunit.hrl").
-endif.

%% API
-export([ start_link/0
        , stop/0
        , init/1

        , get/0
        , get/1
        , set/1
        , set/2
        , merge/1
        ]).

%% gen_server callbacks
-export([ handle_call/3
        , handle_cast/2
        , terminate/2
        , handle_info/2
        , code_change/3
        ]).

-record(state, {config :: map()}).

%%====================================================================
%% API functions
%%====================================================================

-spec start_link() -> {ok, pid()} | ignore | {error, term()}.
start_link() ->
  gen_server:start_link({local, ?MODULE}, ?MODULE, [], []).

-spec get() -> map().
get() ->
  gen_server:call(?MODULE, get).

-spec get(atom()) -> any().
get(Key) ->
  gen_server:call(?MODULE, {get, Key}).

-spec set(atom(), any()) -> ok.
set(Key, Value) ->
  gen_server:call(?MODULE, {set, Key, Value}).

-spec set(map()) -> ok.
set(Config) ->
  gen_server:call(?MODULE, {set, Config}).

-spec merge(map()) -> ok.
merge(Config) ->
  gen_server:call(?MODULE, {merge, Config}).

-spec stop() -> ok.
stop() ->
  gen_server:cast(?MODULE, stop).

%%====================================================================
%% gen_server callbacks
%%====================================================================
init([]) ->
  {ok, #state{config = make_default_config()}}.

handle_call({get, Key}, _From, #state{config = C} = State) ->
  {reply, maps:get(Key, C, undefined), State};
handle_call(get, _From, #state{config = C} = State) ->
  {reply, C, State};
handle_call({set, Config}, _From, #state{config = C} = State) ->
  {reply, ok, State#state{config = update_changed(C, Config)}};
handle_call({set, Key, Value}, _From, #state{config = C} = State) ->
  {reply, ok, State#state{config = maybe_update(Key, Value, C)}};
handle_call({merge, Config}, _From, #state{config = C} = State) ->
  {reply, ok, State#state{config = update_changed(C, Config)}}.

handle_cast(stop, State) ->
  {stop, normal, State}.

handle_info({'EXIT', _Pid, _Reason}, State) ->
  {noreply, State}.

code_change(_OldVsn, State, _Extra) ->
  {ok, State}.

terminate(_Reason, _State) ->
  ok.

%%====================================================================
%% Internal functions
%%====================================================================

update_changed(Config, NewConfig) ->
  maps:fold(fun (Key, Value, Acc) ->
    case maps:get(Key, Config, undefined) of
      undefined ->
        maps:put(Key, Value, Acc);
      Value ->
        Acc;
      _Updated ->
        maybe_update(Key, Value, Acc)
    end
  end, Config, NewConfig).

maybe_update(routes, Value, Config) ->
  Config1 = maps:put(routes, Value, Config),
  maps:put(allowed_methods, extract_allowed_methods(Value), Config1);
maybe_update(allowed_methods, _Value, Config) ->
  Config;
maybe_update(Key, Value, Config) ->
  maps:put(Key, Value, Config).

make_default_config() ->
  DefaultRoutes = [make_default_route()],
  #{  host            => <<"127.0.0.1">>,
      port            => 8089,
      headers         => make_default_headers(),
      routes          => DefaultRoutes,
      allowed_methods => extract_allowed_methods(DefaultRoutes),
      middleware      => #{}
  }.

%% get allowed methods from config.routes
extract_allowed_methods([]) -> [];
extract_allowed_methods(undefined) -> [];
extract_allowed_methods(Routes) ->
  lists:foldl(fun ({M, _, _, _}, Acc) ->
    Method = string:to_upper(?a2l(M)),
    maybe_append(Method, Acc)
  end, [], Routes).

maybe_append(Item, List) ->
  case lists:member(Item, List) of
    true  -> List;
    false -> [Item | List]
  end.

make_default_headers() ->
  [
      ellija_resp:header(allow_origin)
    , ellija_resp:header(content_type_json)
  ].

make_default_route() ->
  {get, <<"/">>, fun default_handler/2, []}.

default_handler(_Req, _Params) ->
  ellija_resp:ok(
      make_default_headers(), {ok, <<"Hello, World!">>}
  ).

%%%============================================================================
%%% Tests
%%%============================================================================

-ifdef(TEST).

config_merge_test() ->
  Config = make_default_config(),
  ?assertEqual(["GET"], maps:get(allowed_methods, Config)),

  Routes = [
      {get,    <<"/list">>,     fun (_, _) -> ellija_resp:ok({ok, <<"list">>}) end, []}
    , {post,   <<"/list">>,     fun (_, _) -> ellija_resp:created() end, []}
    , {get,    <<"/list/:id">>, fun (_, _) -> ellija_resp:ok({ok, <<"detail:1">>}) end, []}
    , {put,    <<"/list/:id">>, fun (_, _) -> ellija_resp:ok({ok, <<"updated:1">>}) end, []}
    , {delete, <<"/list/:id">>, fun (_, _) -> ellija_resp:no_content() end, []}
  ],
  NewConfig = maps:put(routes, Routes, Config),
  MergedConfig = update_changed(Config, NewConfig),

  ?assertEqual(
      lists:sort(["GET", "POST", "PUT", "DELETE"])
    , lists:sort(maps:get(allowed_methods, MergedConfig))
  ),

  ok.

extract_allowed_methods_test() ->
  ?assertEqual([], extract_allowed_methods([])),

  Routes = [
      {get,    <<"/list">>,     fun (_, _) -> ellija_resp:ok({ok, <<"list">>}) end, []}
    , {post,   <<"/list">>,     fun (_, _) -> ellija_resp:created() end, []}
    , {get,    <<"/list/:id">>, fun (_, _) -> ellija_resp:ok({ok, <<"detail:1">>}) end, []}
    , {put,    <<"/list/:id">>, fun (_, _) -> ellija_resp:ok({ok, <<"updated:1">>}) end, []}
    , {delete, <<"/list/:id">>, fun (_, _) -> ellija_resp:no_content() end, []}
  ],
  ?assertEqual(
      lists:sort(["GET", "POST", "PUT", "DELETE"])
    , lists:sort(extract_allowed_methods(Routes))
  ),

  ok.

-endif.