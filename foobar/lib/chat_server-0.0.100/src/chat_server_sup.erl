-module(chat_server_sup).
-behaviour(supervisor).

% API
-export([start_link/0]).

%% Supervisor callbacks
-export([init/1]).

-define(SERVER, ?MODULE).

start_link() ->
	supervisor:start_link({local, ?SERVER}, ?MODULE, []).

init([]) ->
  Children = [
    % Keep alive process to prevent the container from stopping
    {keep_alive, {keep_alive, start_link, []},
     permanent, 5000, worker, [keep_alive]}
  ],

  % One_for_one strategy restarts only the crashed process
  {ok, {{one_for_one, 10, 10}, Children}}.
