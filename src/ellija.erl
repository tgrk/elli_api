%%%----------------------------------------------------------------------------
%%% @doc
%%% Main API
%%% @end
%%%----------------------------------------------------------------------------

-module(ellija).

-include("ellija.hrl").

%% API exports
-export([ start/0
        , start/1
        , stop/0
        ]).

%%====================================================================
%% API functions
%%====================================================================

start() ->
  start(#{port => 8089}).

start(Config) ->
  Port = maps:get(port, Config, 8089),
  case ellija_sup:start_server(Port) of
    {ok, Pid} ->
      ok = ellija_config:merge(Config),
      {ok, Pid};
    {error, Reason} ->
      {error, Reason}
  end.

stop() ->
  ellija_sup:stop_server().

%%====================================================================
%% Internal functions
%%====================================================================
