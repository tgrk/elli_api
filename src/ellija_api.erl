%%%----------------------------------------------------------------------------
%%% @doc
%%%
%%% @end
%%%----------------------------------------------------------------------------
-module(ellija_api).

-behaviour(elli_handler).

-include("ellija.hrl").
-include_lib("kernel/include/file.hrl").

%% API
-export([handle/2, handle_event/3]).

%%%============================================================================
%%% API functions
%%%============================================================================

handle(Req, Args) ->
    Config = ellija_config:get(),
    try
        handle(elli_request:method(Req), elli_request:path(Req), Req, Config)
    catch
        Class:Reason ->
            error_logger:error_msg("Unhandled REST API ~p: ~p: ~p",
                   [Class, Reason, erlang:get_stacktrace()]),
            ellija_resp:internal_error(Req, Config#config.headers)
    end.

handle_event(_, _, _) ->
    ok.

%%%============================================================================
%%% Internal functions
%%%============================================================================
handle(_Method, _Path, Req, #config{headers = Headers}) ->
    {404, Headers, <<"Not Found">>};
handle(_Method, _Path, _Req, #config{headers = Headers}) ->
    {404, Headers, <<"Not Found">>}.