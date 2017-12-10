%%%----------------------------------------------------------------------------
%%% @doc
%%% Holds server configuration
%%% @end
%%%----------------------------------------------------------------------------

-module(ellija_config).

-include("ellija.hrl").

-behaviour(gen_server).

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

start_link() ->
  gen_server:start_link({local, ?MODULE}, ?MODULE, [], []).

get() ->
    gen_server:call(?MODULE, get).

get(Key) ->
  gen_server:call(?MODULE, {get, Key}).

set(Key, Value) ->
    gen_server:call(?MODULE, {set, Key, Value}).

set(Config) ->
    gen_server:call(?MODULE, {set, Config}).

merge(Config) ->
    gen_server:call(?MODULE, {merge, Config}).

stop() ->
  gen_server:cast(?MODULE, stop).

%%====================================================================
%% gen_server callbacks
%%====================================================================
init([]) ->
    {ok, #state{config = make_default_config()}}.

handle_call({get, Key}, _From, State) ->
    {reply, maps:get(Key, State#state.config), State};
handle_call(get, _From, State) ->
    {reply, State#state.config, State};
handle_call({set, NewConfig}, _From, State) ->
    {reply, ok, State#state{config = NewConfig}};
handle_call({set, Key, Value}, _From, State) ->
    NewConfig = maps:put(Key, Value, State#state.config),
    {reply, ok, State#state{config = NewConfig}};
handle_call({merge, NewConfig}, _From, State) ->
    MergedConfig = maps:merge(State#state.config, NewConfig),
    {reply, ok, State#state{config = MergedConfig}}.

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

make_default_config() ->
    #{  host       => <<"127.0.0.1">>,
        port       => 8089,
        headers    => make_default_headers(),
        routes     => [make_default_route()],
        middleware => #{}
    }.

make_default_headers() ->
    [
          ellija_resp:header(allow_origin)
        , ellija_resp:header(content_type_json)
    ].

make_default_route() ->
    {get, <<"/">>, fun default_handler/2, []}.

default_handler(Req, _Params) ->
    ellija_resp:ok(
        Req, make_default_headers(), {ok, <<"Hello, World!">>}
    ).