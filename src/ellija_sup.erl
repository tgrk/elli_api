%%%-------------------------------------------------------------------
%% @doc ellija top level supervisor.
%% @end
%%%-------------------------------------------------------------------

-module(ellija_sup).

-behaviour(supervisor).

-include("ellija.hrl").

%% API
-export([start_link/0
        , start_server/1
        , stop_server/0
        ]).

%% Supervisor callbacks
-export([init/1]).

-define(SERVER, ?MODULE).
-define(CHILD(I, Type, Params),
    {I, {I, start_link, Params}, permanent, 5000, Type, [I]}).

%%====================================================================
%% API functions
%%====================================================================

-spec start_link() -> {ok, pid()} | ignore | {error, term()}.
start_link() ->
    supervisor:start_link({local, ?SERVER}, ?MODULE, []).

-spec start_server(pos_integer()) -> pid() | {error, term()}.
start_server(Port) ->
    case supervisor:start_child(?MODULE, elli_specs(Port)) of
        {error, Reason} ->
            {error, {unable_to_start_server, Reason}};
        {ok, Pid} ->
            Pid
    end.

-spec stop_server() -> ok | {error, term()}.
stop_server() ->
    Port = ellija_config:get(port),
    case supervisor:terminate_child(?MODULE, elli_specs(Port)) of
        ok ->
            supervisor:delete_child(?MODULE, elli_specs(Port));
        Error ->
                Error
    end.

%%====================================================================
%% Supervisor callbacks
%%====================================================================

init([]) ->
    ChildSpecs = [?CHILD(ellija_config, worker, [])],
    {ok, { {one_for_all, 0, 1}, ChildSpecs} }.

%%====================================================================
%% Internal functions
%%====================================================================

elli_specs(Port) when Port >= 80 ->
    Opts = [{callback,      elli_middleware},
            {callback_args, elli_middlewares()},
            {port,          Port}],
    {elli, {elli, start_link, [Opts]}, permanent, 5000, worker, []};
elli_specs(_) ->
    elli_specs(8089).

%%TODO: use built in middlewares
elli_middlewares() ->
    [
        {mods,  [
                    {elli_middleware_compress, []},
                    {elli_cookie,              []},
                    {ellija_api,               []}
                ]
        }
    ].