%%%----------------------------------------------------------------------------
%%% @author Martin Wiso <martin@wiso.cz>
%%% @doc
%%% Helper for handling API responses
%%% @end
%%%----------------------------------------------------------------------------
-module(ellija_resp).

-include("ellija.hrl").
-include_lib("elli/include/elli.hrl").


%% API exports
-export([ options/2

        , ok/2
        , created/1
        , no_content/1
        , bad_request/1
        , bad_request/2
        , unauthorized/2
        , forbidden/1
        , not_found/1
        , conflict/1
        , internal_error/1

        , ok/3
        , created/2
        , no_content/2
        , bad_request/3
        , unauthorized/3
        , forbidden/2
        , not_found/2
        , conflict/2
        , internal_error/2

        , raw/4

        , to_json/1
        , is_json/1
        , has_header/2
        , header/1
        , header/2
        ]).

%%====================================================================
%% API functions
%%====================================================================

-spec ok(#req{}, tuple()) -> tuple().
ok(Req, {ok, Response}) ->
    ok(Req, [], Response).

-spec created(#req{}) -> tuple().
created(Req) ->
    created(Req, []).

-spec no_content(#req{}) -> tuple().
no_content(Req) ->
    no_content(Req, []).

-spec forbidden(#req{}) -> tuple().
forbidden(Req) ->
    forbidden(Req, []).

-spec not_found(#req{}) -> tuple().
not_found(Req) ->
    not_found(Req, []).

-spec conflict(#req{}) -> tuple().
conflict(Req) ->
    conflict(Req, []).

-spec internal_error(#req{}) -> tuple().
internal_error(Req) ->
    internal_error(Req).

-spec bad_request(#req{}, map() | list()) -> tuple().
bad_request(Req, Headers) when is_list(Headers) ->
    raw(Req, 400, Headers, <<"Bad Request">>);
bad_request(Req, ErrorObject) when is_map(ErrorObject) ->
    bad_request(Req, [], ErrorObject).

-spec unauthorized(#req{}, list() | map()) -> tuple().
unauthorized(Req, Headers) when is_list(Headers) ->
    raw(Req, 401, Headers, <<"Unauthorized">>);
unauthorized(Req, ErrorObject) when is_map(ErrorObject) ->
    unauthorized(Req, [], ErrorObject).

-spec bad_request(#req{}) -> tuple().
bad_request(Req) ->
    bad_request(Req, []).

-spec ok(#req{}, list(), tuple()) -> tuple().
ok(Req, Headers, {ok, Response}) ->
    raw(Req, 200, Headers, Response).

-spec created(#req{}, list()) -> tuple().
created(Req, Headers) ->
    raw(Req, 201, Headers, <<>>).

-spec no_content(#req{}, list()) -> tuple().
no_content(Req, Headers) ->
    raw(Req, 204, Headers, <<>>).

-spec bad_request(#req{}, list(), map()) -> tuple().
bad_request(Req, Headers, ErrorObject) ->
    raw(Req, 400, Headers, to_json(ErrorObject)).

-spec unauthorized(#req{}, list(), map()) -> tuple().
unauthorized(Req, Headers, ErrorObject) ->
    raw(Req, 401, Headers, to_json(ErrorObject)).

-spec forbidden(#req{}, list()) -> tuple().
forbidden(Req, Headers) ->
    raw(Req, 403, Headers, <<"Forbidden">>).

-spec not_found(#req{}, list()) -> tuple().
not_found(Req, Headers) ->
    raw(Req, 404, Headers, <<"Not Found">>).

-spec conflict(#req{}, list()) -> tuple().
conflict(Req, Headers) ->
    raw(Req, 409, Headers, <<"Conflict">>).

-spec internal_error(#req{}, list()) -> tuple().
internal_error(Req, Headers) ->
    raw(Req, 500, Headers, <<"Internal Server Error">>).

-spec options(#req{}, list()) -> tuple().
options(Req, AllowedMethods) ->
    Headers =
        [ header(allow_origin)
        , {<<"Access-Control-Allow-Methods">>, format_methods(AllowedMethods)}
        , {<<"Access-Control-Allow-Headers">>, <<"Authorization, X-Requested-With, Accept, Origin, "
             "Content-Type, Content-Encoding">>}
        , {<<"Access-Control-Allow-Credentials">>,
           <<"X-PINGOTHER">>}
        ],
    raw(Req, 200, Headers, <<>>).

-spec raw(#req{}, pos_integer(), list(), binary()) -> tuple().
raw(_Req, Code, [], Body) ->
    raw(_Req, Code, ellija_config:get(headers), Body);
raw(_Req, Code, Headers, Body) ->
    case is_json(Headers) of
        true ->
            {Code, [header(allow_origin) | Headers], jiffy:encode(Body)};
        false ->
            {Code, [header(allow_origin) | Headers], Body}
    end.

-spec to_json(any()) -> binary().
to_json(Struct) ->
    jiffy:encode(Struct).

-spec is_json(list()) -> boolean().
is_json(Headers) ->
    has_header(Headers, <<"application/json; charset=utf-8">>).

-spec has_header(list(), binary()) -> boolean().
has_header(Headers, Key) ->
    proplists:get_value(Key, Headers, <<>>) =/= <<>>.

-spec header(atom()) -> tuple().
header(allow_origin) ->
    header(allow_origin, <<"*">>);
header(content_type_json) ->
    header(content_type, <<"application/json; charset=utf-8">>).

-spec header(atom(), binary() | any()) -> tuple().
header(allow_credentials, Value) ->
    {<<"Access-Control-Allow-Credentials">>, ellija_utils:to_bin(Value)};
header(content_type, Value) ->
    {<<"Content-Type">>, ellija_utils:to_bin(Value)};
header(allow_origin, Value) ->
    {<<"Access-Control-Allow-Origin">>, ellija_utils:to_bin(Value)}.

%%====================================================================
%% Internal functions
%%====================================================================
format_methods(Methods) ->
    string:join([ellija_utils:to_list(M) || M <- Methods], ", ").
