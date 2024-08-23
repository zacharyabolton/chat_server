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

start_link() ->
	supervisor:start_link({local, ?SERVER}, ?MODULE, []).

init([]) ->
    Processes = [],
    {ok, {{one_for_one, 1, 5}, Processes}}.
