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
  application:get_all_env(chat_server),
  application:ensure_all_started(chat_server),
  RunningApps = application:which_applications(),
  [{running_apps, RunningApps} | Config].

end_per_suite(_Config) ->
  application:stop(chat_server),
  ok.

init_per_testcase(_TestCase, Config) ->
  Config.

end_per_testcase(_TestCase, _Config) ->
  ok.

test_chat_server_starts(Config) ->
  RunningApps = ?config(running_apps, Config),
  ?assert(lists:keymember(chat_server, 1, RunningApps)),
  SupervisorPid = whereis(chat_server_sup),
  ?assert(is_pid(SupervisorPid)).
