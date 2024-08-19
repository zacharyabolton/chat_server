%%%-------------------------------------------------------------------
%% @doc chat_server public API
%% @end
%%%-------------------------------------------------------------------

-module(chat_server_app).
-behaviour(application).
%% Application callbacks
-export([start/2, stop/1]).

%%====================================================================
%% API
%%====================================================================

start(_Type, _Args) ->
    io:format("Starting chat_server_app~n"),
    {ok, _} = application:ensure_all_started(cowboy),
    io:format("Cowboy started~n"),
    Dispatch = cowboy_router:compile([
        {'_', [{"/", hello_handler, []}]}
    ]),
    io:format("Dispatch compiled~n"),
    case cowboy:start_clear(my_http_listener,
        [{port, 8080}],
        #{env => #{dispatch => Dispatch}}
    ) of
        {ok, _} ->
            io:format("Cowboy HTTP listener started on port 8080~n");
        Error ->
            io:format("Failed to start Cowboy HTTP listener: ~p~n", [Error])
    end,
    Result = chat_server_sup:start_link(),
    io:format("chat_server_sup:start_link() result: ~p~n", [Result]),
    Result.

stop(_State) ->
    io:format("Stopping chat_server_app~n"),
    ok = cowboy:stop_listener(my_http_listener),
    ok.
