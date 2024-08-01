%%%-------------------------------------------------------------------
%% @doc chat_server public API
%% @end
%%%-------------------------------------------------------------------

-module(chat_server_app).

-behaviour(application).

%% Application callbacks
-export([start/2, stop/1]).

start(_StartType, _StartArgs) ->
    Server_Node = config:get_env("SERVER_NODE", "default@localhost"),
    io:format("Starting server on node: ~p~n", [Server_Node]),
    chat_server_sup:start_link().

stop(_State) ->
    ok.

%% internal functions
