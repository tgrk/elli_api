%%%----------------------------------------------------------------------------
%%% @author Martin Wiso <martin@wiso.cz>
%%% @doc
%%% Helper for handling API requests
%%% @end
%%%----------------------------------------------------------------------------
-module(ellija_req).

-include("ellija.hrl").
-include_lib("elli/include/elli.hrl").

%% API exports
-export([ parse_json_body/1
        , parse_header_value/2

        , get_method/1

        , to_qs_args/1
        , parse_args/1
        ]).

%%====================================================================
%% API functions
%%====================================================================
-spec parse_json_body(#req{}) -> map().
parse_json_body(Req) ->
  case elli_request:body(Req) of
    <<>> -> #{};
    Body -> jiffy:decode(unicode:characters_to_binary(Body), [return_maps])
  end.

-spec parse_header_value(list(), string()) -> list(string()).
parse_header_value(Headers, Key) ->
  string:tokens(proplists:get_value(Key, Headers, ""), ";").

-spec get_method(#req{}) -> binary().
get_method(Req) ->
  ?a2b(elli_request:method(Req)).

-spec to_qs_args(list({any(), any()}) | map()) -> string().
to_qs_args(Args) when is_map(Args) ->
  to_qs_args(maps:to_list(Args));
to_qs_args(Args) ->
  string:join(
    [ellija_utils:to_list(K) ++ "=" ++ http_uri:encode(ab_utils:to_list(V))
      || {K, V} <- Args], "&").

-spec parse_args(#req{}) -> list().
parse_args(Req) ->
  case elli_request:method(Req) of
    'GET'     -> elli_request:get_args_decoded(Req);
    'DELETE'  -> elli_request:get_args_decoded(Req);
    'OPTIONS' -> [];
    _         -> []
  end.

%%====================================================================
%% Internal functions
%%====================================================================
