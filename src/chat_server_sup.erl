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
    SupFlags = #{strategy => one_for_one,
                 intensity => 10,
                 period => 10},
    ChildSpecs = [#{id => keep_alive,
                    start => {keep_alive, start_link, []},
                    restart => permanent,
                    shutdown => 5000,
                    type => worker,
                    modules => [keep_alive]}],
    {ok, {SupFlags, ChildSpecs}}.
