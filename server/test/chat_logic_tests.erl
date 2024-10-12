%%%-------------------------------------------------------------------
%% @doc chat_logic_tests.erl
%% Tests for the chat logic
%% @end
%%%-------------------------------------------------------------------

-module(chat_logic_tests).
-include_lib("eunit/include/eunit.hrl").

%%% Setup and teardown helpers for tests

setup() ->
    meck:new(connection_manager, [passthrough]),
    ok.

teardown() ->
    %% Clean up mock
    meck:unload(connection_manager),
    ok.

%% Test cases

% Test the successful routing of a message
process_message_test() ->
    setup(),

    %% Mock connection_manager and get the PID
    meck:expect(connection_manager, route_message, fun(_, _) -> ok end),

    %% Test process_message/1
    Result = chat_logic:process_message(self(), "Test message"),

    %% Assert that the message was successfully routed
    ?assertEqual(ok, Result),

    teardown().

% Test routing when no clients are available
process_message_no_clients_test() ->
    setup(),

    %% Simulate no clients available
    meck:expect(connection_manager, route_message, fun(_, _) -> {error, no_clients} end),

    %% Test process_message/1
    Result = chat_logic:process_message(self(), "Test message"),

    %% Assert that the error is handled properly
    ?assertEqual({error, no_clients}, Result),

    teardown().
