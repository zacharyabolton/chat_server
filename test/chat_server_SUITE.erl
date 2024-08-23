%%%-------------------------------------------------------------------
%% @doc chat_server_SUITE.erl
%% Common Test suite for chat_server
%% @end
%%%-------------------------------------------------------------------

-module(chat_server_SUITE).
-include_lib("common_test/include/ct.hrl").
-include_lib("eunit/include/eunit.hrl").

-export([all/0, init_per_suite/1, end_per_suite/1, init_per_testcase/2, end_per_testcase/2]).
-export([test_chat_server_starts/1]).

all() -> [test_chat_server_starts].

init_per_suite(Config) ->
    application:load(chat_server),
    ConfigPath = config_path(),
    ct:pal("Config path: ~p", [ConfigPath]),
    case file:consult(ConfigPath) of
        {ok, [AppConfig]} ->
            [application:set_env(chat_server, K, V) || {K, V} <- proplists:get_value(chat_server, AppConfig)],
            Config;
        {error, Reason} ->
            ct:pal("Failed to load config: ~p", [Reason]),
            {skip, {config_load_failed, Reason}}
    end.

end_per_suite(_Config) ->
    application:stop(chat_server),
    ok.

init_per_testcase(_TestCase, Config) ->
    Config.

end_per_testcase(_TestCase, _Config) ->
    application:stop(chat_server),
    ok.

test_chat_server_starts(_Config) ->
    ct:pal("Starting chat_server application"),
    ct:pal("Environment before start: ~p", [application:get_all_env(chat_server)]),
    ct:pal("Current working directory: ~p", [file:get_cwd()]),
    ct:pal("Contents of current directory: ~p", [file:list_dir(".")]),

    case application:ensure_all_started(chat_server) of
        {ok, StartedApps} ->
            ct:pal("Started applications: ~p", [StartedApps]);
        {error, Reason} ->
            ct:fail("Failed to start chat_server: ~p", [Reason])
    end,

    timer:sleep(1000), % Give it a moment to start up

    RunningApps = application:which_applications(),
    ct:pal("Running applications: ~p", [RunningApps]),

    ?assert(lists:keymember(chat_server, 1, RunningApps)),

    SupervisorPid = whereis(chat_server_sup),
    ct:pal("Supervisor PID: ~p", [SupervisorPid]),
    ?assert(is_pid(SupervisorPid)),

    Port = application:get_env(chat_server, port, undefined),
    ct:pal("Configured port: ~p", [Port]),
    ?assertEqual(8081, Port).

config_path() ->
    Paths = [
        filename:join([code:lib_dir(chat_server), "config", "sys_test.config"]),
        filename:join(["..", "config", "sys_test.config"]),
        filename:join(["config", "sys_test.config"])
    ],
    case lists:filter(fun filelib:is_file/1, Paths) of
        [Path|_] -> Path;
        [] ->
            ct:pal("Could not find sys_test.config. Searched paths: ~p", [Paths]),
            throw({config_not_found, Paths})
    end.
