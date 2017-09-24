%%%----------------------------------------------------------------------------
%%% @author Martin Wiso <martin@wiso.cz>
%%% @doc
%%% Main API
%%% @end
%%%----------------------------------------------------------------------------
-module(elli_api).

%% API exports
-export([handle/3]).

-compile({no_auto_import, [get/1, put/2]}).

%%====================================================================
%% API functions
%%====================================================================
handle(Req, Args, Routes) ->
    Method = elli_api_req:get_method(Req),
    case map:get(Method, Routes, <<>>) of
        <<>> ->
            not_implement;
        Route ->
            Path = elli_request:path(Req),
            not_implement
    end.

%%====================================================================
%% Internal functions
%%====================================================================
