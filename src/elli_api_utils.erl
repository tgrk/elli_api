%%%----------------------------------------------------------------------------
%%% @author Martin Wiso <martin@wiso.cz>
%%% @doc
%%% Shared utilities
%%% @end
%%%----------------------------------------------------------------------------
-module(elli_api_utils).

-include("elli_api.hrl").
-include_lib("elli/include/elli.hrl").

%% API exports
-export([ to_list/1
        , to_atom/1
        , to_bin/1
        ]).

%%====================================================================
%% API functions
%%====================================================================
%% Type conversion helpers
-spec to_list(any()) -> string().
to_list(B) when is_binary(B) ->
    ?b2l(B);
to_list(N) when is_integer(N) ->
    ?i2l(N);
to_list(A) when is_atom(A) ->
    ?a2l(A);
to_list(L) ->
    L.

-spec to_atom(any()) -> atom().
to_atom(L) when is_list(L) ->
    ?l2a(L);
to_atom(N) when is_integer(N) ->
    ?i2a(N);
to_atom(B) when is_binary(B) ->
    ?b2a(B);
to_atom(A) ->
    A.

-spec to_bin(any()) -> binary().
to_bin(undefined) ->
    <<>>;
to_bin(B) when is_binary(B) ->
    B;
to_bin(N) when is_integer(N) ->
    ?i2b(N);
to_bin(A) when is_atom(A) ->
    ?a2b(A);
to_bin(L) when is_list(L) ->
    unicode:characters_to_binary(L, utf8).


%%====================================================================
%% Internal functions
%%====================================================================
