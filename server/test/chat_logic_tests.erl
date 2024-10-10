%%%-------------------------------------------------------------------
%% @doc chat_logic_tests.erl
%% Tests for the chat logic
%% @end
%%%-------------------------------------------------------------------

-module(chat_logic_tests).
-include_lib("eunit/include/eunit.hrl").

%% Test cases

%% Test the successful routing of a message
process_message_test() ->
    %% Mock connection_manager
    meck:new(connection_manager, [passthrough]),
    meck:expect(connection_manager, route_message, fun(_, _) -> ok end),

    %% Test process_message/1
    Result = chat_logic:process_message("Test message"),

    %% Assert that the message was successfully routed
    ?assertEqual(ok, Result),

    %% Clean up mock
    meck:unload(connection_manager).

%% Test routing when no clients are available
process_message_no_clients_test() ->
    %% Simulate no clients available
    meck:new(connection_manager, [passthrough]),
    meck:expect(connection_manager, route_message, fun(_, _) -> {error, no_clients} end),

    %% Test process_message/1
    Result = chat_logic:process_message("Test message"),

    %% Assert that the error is handled properly
    ?assertMatch({error, no_clients}, Result),

    %% Clean up mock
    meck:unload(connection_manager).

%% Test client registration
register_client_test() ->
    %% Mock client registration
    meck:new(connection_manager, [passthrough]),
    meck:expect(connection_manager, add_client, fun(_ClientPid) -> ok end),

    %% Test client registration
    Pid = self(),
    chat_logic:register_client(Pid),

    %% Verify that connection_manager:add_client/1 was called
    ?assert(meck:called(connection_manager, add_client, [Pid])),

    %% Clean up mock
    meck:unload(connection_manager).

%% Test client deregistration
deregister_client_test() ->
    %% Mock client deregistration
    meck:new(connection_manager, [passthrough]),
    meck:expect(connection_manager, remove_client, fun(_ClientPid) -> ok end),

    %% Test client deregistration
    Pid = self(),
    chat_logic:deregister_client(Pid),

    %% Verify that connection_manager:remove_client/1 was called
    ?assert(meck:called(connection_manager, remove_client, [Pid])),

    %% Clean up mock
    meck:unload(connection_manager).
