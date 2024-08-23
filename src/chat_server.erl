%%%-------------------------------------------------------------------
%% @doc chat_server.erl
%% Main application module
%% @end
%%%-------------------------------------------------------------------

-module(chat_server).
-behaviour(application).
%% Application callbacks
-export([start/2, stop/1]).

%%====================================================================
%% API
%%====================================================================

start(_Type, _Args) ->
    io:format("Starting chat server~n"),
    Dispatch = router:dispatch(),
    io:format("Dispatch: ~p~n", [Dispatch]),
    Port = get_port(),
    io:format("Starting cowboy on port ~p~n", [Port]),
    {ok, _} = cowboy:start_clear(http_listener,
        [{port, Port}],
        #{env => #{dispatch => Dispatch}}
    ),
    chat_server_sup:start_link().

stop(_State) ->
    ok = cowboy:stop_listener(http_listener).

%%====================================================================
%% Internal functions
%%====================================================================

get_port() ->
  application:get_env(chat_server, port, 8081).