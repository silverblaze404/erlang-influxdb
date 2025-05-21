-module(influxdb_http).

-export([post/7]).

-export_type([result/0, series/0]).

-spec post(client(), binary(), string(), string(), string(), iodata(), timeout()) ->
    ok
    | {ok, [result()]}
    | {error, {not_found, string()}}
    | {error, {server_error, string()}}.
-type client() :: query | write.
-type result() :: [series()].
-type series() ::
    #{
        name := binary(),
        columns := [binary()],
        rows := [tuple()],
        tags => #{binary() => binary()}
    }.

post(Client, Url, Username, Password, ContentType, Body, Timeout) ->
    case application:get_env(influxdb, http_client, hackney) of
        httpc ->
            post_httpc(Client, Url, Username, Password, ContentType, Body, Timeout);
        hackney ->
            post_hackney(Client, Url, Username, Password, ContentType, Body, Timeout);
        _ ->
            erlang:error({badarg, <<"Invalid HTTP client">>})
    end.

post_hackney(Client, Url, Username, Password, ContentType, Body, Timeout) ->
    Authorization = "Basic " ++ base64:encode_to_string(Username ++ ":" ++ Password),
    Headers = [
        {"Authorization", Authorization}, {"Content-Type", ContentType}
    ],
    Options = [
        {timeout, Timeout},
        {recv_timeout, Timeout},
        {connect_timeout, Timeout},
        {pool, profile(Client)}
    ],
    RetryMax = 5,
    RetryDelay = 500, % Initial delay in milliseconds
    post_hackney_with_retries(Url, Headers, Body, Options, RetryMax, RetryDelay).

post_hackney_with_retries(_Url, _Headers, _Body, _Options, 0, _) ->
    erlang:exit({error, max_retries_exceeded});
post_hackney_with_retries(Url, Headers, Body, Options, RetriesLeft, Delay) ->
    case hackney:request(post, Url, Headers, Body, Options) of
        {ok, StatusCode, RespHeaders, ClientRef} ->
            case hackney:body(ClientRef) of
                {ok, RespBody} ->
                    response(StatusCode, RespHeaders, RespBody);
                {error, _Reason} ->
                    timer:sleep(Delay),
                    post_hackney_with_retries(Url, Headers, Body, Options, RetriesLeft - 1, Delay * 2)
            end;
        {ok, Status, _Headers} when Status == 200 ->
            ok;
        {ok, Status, _Headers} ->
            case is_retriable_status(Status) of
                true ->
                    timer:sleep(Delay),
                    post_hackney_with_retries(Url, Headers, Body, Options, RetriesLeft - 1, Delay * 2);
                false ->
                    erlang:exit({bad_response, Status})
            end;
        {ok, ClientRef} ->
            {ok, {async, ClientRef}};
        {error, _Reason} ->
            timer:sleep(Delay),
            post_hackney_with_retries(Url, Headers, Body, Options, RetriesLeft - 1, Delay * 2)
    end.

post_httpc(Client, Url, Username, Password, ContentType, Body, Timeout) ->
    Authorization = "Basic " ++ base64:encode_to_string(Username ++ ":" ++ Password),
    Headers = [{"Authorization", Authorization}],
    case
        httpc:request(
            post,
            {binary_to_list(Url), Headers, ContentType, iolist_to_binary(Body)},
            [{timeout, Timeout}],
            [{body_format, binary}],
            profile(Client)
        )
    of
        {ok, {{_, RespCode, _}, RespHeaders, RespBody}} ->
            response(RespCode, RespHeaders, RespBody);
        {error, Reason} ->
            erlang:exit(Reason)
    end.

%% Internals

is_retriable_status(Status) ->
    lists:member(Status, [408, 429, 500, 502, 503, 504]).

profile(query) ->
    influxdb_query;
profile(write) ->
    influxdb_write.

response(200, _, Body) ->
    case results(jsone:decode(Body)) of
        [] ->
            ok;
        Results ->
            {ok, Results}
    end;
response(204, _, _) ->
    ok;
response(400, _, Body) ->
    #{<<"error">> := Message} = jsone:decode(Body),
    erlang:error({bad_request, unicode:characters_to_list(Message)});
response(404, _, Body) ->
    #{<<"error">> := Message} = jsone:decode(Body),
    {error, {not_found, unicode:characters_to_list(Message)}};
response(500, _, Body) ->
    #{<<"error">> := Message} = jsone:decode(Body),
    {error, {server_error, unicode:characters_to_list(Message)}}.

results(#{<<"results">> := Results}) ->
    [series(Series) || #{<<"series">> := Series} <- Results].

series(Series) ->
    [
        maps:fold(
            fun
                (<<"name">>, Name, Acc) ->
                    Acc#{name => Name};
                (<<"tags">>, Tags, Acc) ->
                    Acc#{tags => Tags};
                (<<"columns">>, Columns, Acc) ->
                    Acc#{columns => Columns};
                (<<"values">>, Values, Acc) ->
                    Acc#{rows => [list_to_tuple(Value) || Value <- Values]}
            end,
            #{},
            S
        )
     || S <- Series
    ].
