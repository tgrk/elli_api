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
        ]).

%% gen_server callbacks
-export([ handle_call/3
        , handle_cast/2
        , terminate/2
        , handle_info/2
        , code_change/3
        ]).

-record(state, {config :: #config{}}).

%%====================================================================
%% API functions
%%====================================================================

start_link() ->
  gen_server:start_link({local, ?MODULE}, ?MODULE, [], []).

get() ->
    gen_server:call(?MODULE, get).

get(Key) ->
  gen_server:call(?MODULE, {get, Key}).

set(Config) ->
    gen_server:call(?MODULE, {set, Config}).

stop() ->
  gen_server:cast(?MODULE, stop).

%%====================================================================
%% gen_server callbacks
%%====================================================================
init([]) ->
    {ok, #state{config = make_default_config()}}.

handle_call({get, Key}, _From, State) ->
    {reply, find(State#state.config, Key), State};
handle_call(get, _From, State) ->
    {reply, State#state.config, State};
handle_call({set, NewConfig}, _From, State) ->
    {reply, ok, State#state{config = NewConfig}}.

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
        #config{
            host = <<"127.0.0.1">>,
            port = 8089,
            headers = make_default_headers(),
            routes = [make_default_route()],
            middleware = #{}
        }.

make_default_headers() ->
    [
          ellijs_resp:header(allow_origin)
        , ellijs_resp:header(content_type_json)
    ].

make_default_route() ->
    {get, "/", fun default_handler/1, []}.

default_handler(Req) ->
    ellija_resp:ok(
        Req, make_default_headers(), {ok, <<"Hello, World!">>}
    ).

find(Config, Key) ->
    [_Tag | Values] = tuple_to_list(Config),
    PL = lists:zip(record_info(fields, config), Values),
    proplists:get_value(Key, PL, undefined).