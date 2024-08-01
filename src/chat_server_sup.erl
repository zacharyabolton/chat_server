%%%-------------------------------------------------------------------
%% @doc chat_server top level supervisor.
%% @end
%%%-------------------------------------------------------------------

-module(chat_server_sup).

-behaviour(supervisor).

%% API
-export([start_link/0]).
%% Supervisor callbacks
-export([init/1]).

-define(SERVER, ?MODULE).

start_link() ->
    supervisor:start_link({local, ?SERVER}, ?MODULE, []).

init([]) ->
    SupFlags = #{strategy => one_for_all,
                 intensity => 1,
                 period => 5},
    ChildSpecs = [
        #{id => chat_server,
          start => {chat_server, start_link, []},
          restart => permanent,
          shutdown => 5000,
          type => worker,
          modules => [chat_server]}
    ],
    {ok, {SupFlags, ChildSpecs}}.
