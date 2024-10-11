%%%-------------------------------------------------------------------
%% @doc chat_session_tests.erl
%% Tests for the chat_session module
%% @end
%%%-------------------------------------------------------------------

-module(chat_session_tests).

-include_lib("eunit/include/eunit.hrl").

%%% Setup and teardown helpers for tests

setup() ->
    ok.

teardown(_) ->
    ok.

%%% Test cases

chat_session_test_() ->
    {foreach,
     fun setup/0,
     fun teardown/1,
     [
      fun test_start_session/1,
      fun test_stop_session/1,
      fun test_add_client/1,
      fun test_remove_client/1,
      fun test_multiple_clients/1,
      fun test_session_isolation/1
     ]}.

test_start_session(_) ->
    SessionId = test_session_1,
    Result = chat_session:start_session(SessionId),
    [
     ?_assertMatch({ok, SessionId}, Result),
     ?_assert(ets:info(SessionId) =/= undefined)
    ].

test_stop_session(_) ->
    SessionId = test_session_2,
    {ok, _} = chat_session:start_session(SessionId),
    Result = chat_session:stop_session(SessionId),
    [
     ?_assertEqual(ok, Result),
     ?_assertEqual(undefined, ets:info(SessionId))
    ].

test_add_client(_) ->
    SessionId = test_session_3,
    {ok, _} = chat_session:start_session(SessionId),
    ClientPid = self(),
    chat_session:add_client(SessionId, ClientPid),
    [
     ?_assertMatch([{ClientPid, _Timestamp}], ets:tab2list(SessionId))
    ].

test_remove_client(_) ->
    SessionId = test_session_4,
    {ok, _} = chat_session:start_session(SessionId),
    ClientPid = self(),
    chat_session:add_client(SessionId, ClientPid),
    chat_session:remove_client(SessionId, ClientPid),
    [
     ?_assertEqual([], ets:tab2list(SessionId))
    ].

test_multiple_clients(_) ->
    SessionId = test_session_5,
    {ok, _} = chat_session:start_session(SessionId),
    ClientPids = [spawn(fun() -> ok end) || _ <- lists:seq(1, 5)],
    [chat_session:add_client(SessionId, Pid) || Pid <- ClientPids],
    Result = ets:tab2list(SessionId),
    [
     ?_assertEqual(5, length(Result)),
     ?_assertEqual(lists:sort(ClientPids), lists:sort([Pid || {Pid, _} <- Result]))
    ].

test_session_isolation(_) ->
    SessionId1 = test_session_6,
    SessionId2 = test_session_7,
    {ok, _} = chat_session:start_session(SessionId1),
    {ok, _} = chat_session:start_session(SessionId2),
    ClientPid1 = spawn(fun() -> ok end),
    ClientPid2 = spawn(fun() -> ok end),
    chat_session:add_client(SessionId1, ClientPid1),
    chat_session:add_client(SessionId2, ClientPid2),
    [
     ?_assertMatch([{ClientPid1, _}], ets:tab2list(SessionId1)),
     ?_assertMatch([{ClientPid2, _}], ets:tab2list(SessionId2))
    ].
