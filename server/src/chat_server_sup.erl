%%%-------------------------------------------------------------------
%% @doc chat_server_sup.erl
%% Supervisor module
%% @end
%%%-------------------------------------------------------------------

-module(chat_server_sup).
-behaviour(supervisor).

% API
-export([start_link/0]).

%% Supervisor callbacks
-export([init/1]).

-define(SERVER, ?MODULE).

%% Start the supervisor
start_link() ->
	supervisor:start_link({local, ?SERVER}, ?MODULE, []).

%% Initialization function defining the child processes
init([]) ->
    %% List of child processes to supervise
    Processes = [
        %% WebSocket handler
        {chat_server_websocket_handler,
         {chat_server_websocket_handler,
          start_link,
          []},
         permanent,
         5000,
         worker,
         [chat_server_websocket_handler]}
    ],
    {ok, {{one_for_one, 1, 5}, Processes}}.
