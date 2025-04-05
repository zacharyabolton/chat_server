%%%-------------------------------------------------------------------
%% @doc chat_server_sup.erl
%% Supervisor module
%% @end
%%%-------------------------------------------------------------------

-module(chat_server_sup).
-behaviour(supervisor).

-export([start_link/0]).
-export([init/1]).

start_link() ->
    supervisor:start_link({local, ?MODULE}, ?MODULE, []).

init([]) ->
    Processes = [
        %% Start the connection_manager
        {connection_manager,
         {connection_manager, start_link, []},
         permanent,
         5000,
         worker,
         [connection_manager]}
    ],
    {ok, {{one_for_one, 1, 5}, Processes}}.
