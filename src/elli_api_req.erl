%%%----------------------------------------------------------------------------
%%% @author Martin Wiso <martin@wiso.cz>
%%% @doc
%%% Helper for handling API requests
%%% @end
%%%----------------------------------------------------------------------------
-module(elli_api_req).

-include("elli_api.hrl").
-include_lib("elli/include/elli.hrl").

%% API exports
-export([ parse_json_body/1
        , parse_header_value/2

        , get_method/1

        , http_flatten_args/1
        , http_args/1
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

-spec http_flatten_args(list({any(), any()}) | map()) -> string().
http_flatten_args(Args) when is_map(Args) ->
    http_flatten_args(maps:to_list(Args));
http_flatten_args(Args) ->
    string:join(
        [ab_utils:to_list(K) ++ "=" ++ http_uri:encode(ab_utils:to_list(V))
         || {K, V} <- Args], "&").

-spec http_args(#req{}) -> list().
http_args(Req) ->
    case elli_request:method(Req) of
        'GET'     -> elli_request:get_args_decoded(Req);
        'DELETE'  -> elli_request:get_args_decoded(Req);
        'OPTIONS' -> [];
        _         -> []
    end.

%%====================================================================
%% Internal functions
%%====================================================================
