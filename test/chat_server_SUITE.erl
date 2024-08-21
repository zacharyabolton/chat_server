-module(chat_server_SUITE).
-include_lib("common_test/include/ct.hrl").
-include_lib("eunit/include/eunit.hrl").

-export([all/0, init_per_suite/1, end_per_suite/1, init_per_testcase/2, end_per_testcase/2]).
% -export([test_chat_server_app_starts/1, test_keep_alive_process/1, test_hello_handler/1]).
-export([test_chat_server_app_starts/1]).

% all() -> [test_chat_server_app_starts, test_keep_alive_process, test_hello_handler].
all() -> [test_chat_server_app_starts].

init_per_suite(Config) ->
    ct:log("Starting chat_server application"),

    ct:log("Loading chat_server application"),
    LoadResult = application:load(chat_server),
    ct:log("Load result: ~p", [LoadResult]),

    ct:log("Getting chat_server environment"),
    Env = application:get_all_env(chat_server),
    ct:log("Application environment: ~p", [Env]),

    ct:log("Starting chat_server application"),
    StartResult = application:ensure_all_started(chat_server),
    ct:log("Start result: ~p", [StartResult]),

    timer:sleep(1000),
    RunningApps = application:which_applications(),
    ct:log("Running applications: ~p", [RunningApps]),
    [{running_apps, RunningApps} | Config].

end_per_suite(_Config) ->
    application:stop(chat_server),
    ok.

init_per_testcase(_TestCase, Config) ->
    Config.

end_per_testcase(_TestCase, _Config) ->
    ok.

test_chat_server_app_starts(Config) ->
    % RunningApps = ?config(running_apps, Config),
    % ct:log("Running apps in test: ~p", [RunningApps]),
    % ?assert(lists:keymember(chat_server, 1, RunningApps)),
    % SupervisorPid = whereis(chat_server_sup),
    % ct:log("chat_server_sup pid: ~p", [SupervisorPid]),
    % ?assert(is_pid(SupervisorPid)).
    ?assert(false).

% test_keep_alive_process(_Config) ->
%     KeepAlivePid = whereis(keep_alive),
%     ct:log("keep_alive pid: ~p", [KeepAlivePid]),
%     ?assert(is_pid(KeepAlivePid)),
%     ?assert(is_process_alive(KeepAlivePid)).
%
% test_hello_handler(_Config) ->
%     ct:log("Attempting to connect to http://localhost:8080/"),
%     case httpc:request(get, {"http://localhost:8080/", []}, [], []) of
%         {ok, {{_, 200, _}, Headers, Body}} ->
%             ct:log("Successful connection. Body: ~p, Headers: ~p", [Body, Headers]),
%             ?assertEqual("Hello Erlang!", Body),
%             ?assertEqual("text/plain", proplists:get_value("content-type", Headers));
%         Other ->
%             ct:log("Failed connection. Result: ~p", [Other]),
%             ?assert(false)
%     end.
