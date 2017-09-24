%%%----------------------------------------------------------------------------
%%% @author Martin Wiso <martin@wiso.cz>
%%% @doc
%%% Helper for handling API responses
%%% @end
%%%----------------------------------------------------------------------------
-module(elli_api_resp).

-include("elli_api.hrl").
-include_lib("elli/include/elli.hrl").


%% API exports
-export([ options/2

        , response/3
        , raw_response/4

        , is_json/1
        , has_header/2
        , header/1
        , header/2
        ]).

%%====================================================================
%% API functions
%%====================================================================
-spec response(#req{}, list(), atom() | tuple()) -> tuple().
response(Req, Headers, {ok, Response}) ->
    raw_response(Req, 200, Headers, Response);
response(Req, Headers, created) ->
    raw_response(Req, 201, Headers, <<>>);
response(Req, Headers, no_content) ->
    raw_response(Req, 204, Headers, <<>>);
response(Req, Headers, {error, bad_request}) ->
    raw_response(Req, 400, Headers, <<"Bad Request">>);
response(Req, Headers, {error, bad_request, Reason}) ->
    raw_response(Req, 400, Headers, Reason);
response(Req, Headers, {error, unauthorized, ErrorObject}) ->
    raw_response(Req, 401, Headers, ErrorObject);
response(Req, Headers, {error, forbidden}) ->
    raw_response(Req, 403, Headers, <<"Forbidden">>);
response(Req, Headers, {error, not_found}) ->
    raw_response(Req, 404, Headers, <<"Not Found">>);
response(Req, Headers, {error, confict}) ->
    raw_response(Req, 409, Headers, <<"Conflict">>);
response(Req, Headers, {error, internal_error}) ->
    raw_response(Req, 500, Headers, <<"Internal Server Error">>);
response(Req, Headers, {error, unknown}) ->
    raw_response(Req, 500, Headers, <<"Internal Server Error">>);
response(Req, Headers, error) ->
    raw_response(Req, 400, Headers, <<"Bad Request">>).

-spec options(#req{}, list()) -> tuple().
options(Req, AllowedMethods) ->
    Headers =
        [ header(allow_origin)
        , {<<"Access-Control-Allow-Methods">>,
           format_methods(AllowedMethods)}
        , {<<"Access-Control-Allow-Headers">>,
           <<"Authorization, X-Requested-With, Accept, Origin, "
             "Content-Type, Content-Encoding">>}
        , {<<"Access-Control-Allow-Credentials">>,
           <<"X-PINGOTHER">>}
        ],
    raw_response(Req, 200, Headers, <<>>).

-spec raw_response(#req{}, pos_integer(), list(), binary()) -> tuple().
raw_response(Req, Code, Headers, Body) ->
    case is_json(Headers) of
        true ->
            {Code, [header(allow_origin) | Headers], jiffy:encode(Body)};
        false ->
            {Code, [header(allow_origin) | Headers], Body}
    end.

-spec is_json(list()) -> boolean().
is_json(Headers) ->
    has_header(Headers, <<"application/json; charset=utf-8">>).

-spec has_header(list(), binary()) -> boolean().
has_header(Headers, Key) ->
    proplistS:get_value(Key, Headers, <<>>) =/= <<>>.

-spec header(atom()) -> tuple().
header(allow_origin) ->
    header(allow_origin, <<"*">>);
header(content_type_json) ->
    header(content_type, <<"application/json; charset=utf-8">>).

-spec header(atom(), binary() | any()) -> tuple().
header(allow_credentials, Value) ->
    {<<"Access-Control-Allow-Credentials">>, elli_api_utils:to_bin(Value)};
header(content_type, Value) ->
    {<<"Content-Type">>, elli_api_utils:to_bin(Value)};
header(allow_origin, Value) ->
    {<<"Access-Control-Allow-Origin">>, elli_api_utils:to_bin(Value)}.

%%====================================================================
%% Internal functions
%%====================================================================
format_methods(Methods) ->
    string:join([elli_api_utils:to_list(M) || M <- Methods], ", ").
