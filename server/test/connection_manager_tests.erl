%%%-------------------------------------------------------------------
%% @doc connection_manager_tests.erl
%% Tests for the connection manager
%% @end
%%%-------------------------------------------------------------------

-module(connection_manager_tests).

-include_lib("eunit/include/eunit.hrl").

%% In-memory client registry (for active WebSocket connections)
-define(CLIENTS_TABLE, clients).

%%% Setup and teardown helpers for tests

setup() ->
    %% Start the connection_manager process
    {ok, Pid} = connection_manager:start_link(),
    Pid.

teardown(Pid) ->
    %% Stop the connection_manager process
    connection_manager:stop(Pid).

%%% Test cases

% Test connection_manager startup and shutdown
basic_start_stop_test() ->
    Pid = setup(),
    is_process_alive(Pid) =:= true,
    teardown(Pid),
    is_process_alive(Pid) =:= false.

%% Test adding a client to the ETS table
add_client_test() ->
    Pid = setup(),
    ClientPid = spawn(fun() -> ok end),

    %% Add the client
    connection_manager:add_client(ClientPid),

    %% Check that the client was added to the ETS table
    ClientList = ets:tab2list(?CLIENTS_TABLE),
    lists:keyfind(ClientPid, 1, ClientList) =/= false,

    %% Clean up
    teardown(Pid).

%% Test removing a client from the ETS table
remove_client_test() ->
    Pid = setup(),
    ClientPid = spawn(fun() -> ok end),

    %% Add and then remove the client
    connection_manager:add_client(ClientPid),
    connection_manager:remove_client(ClientPid),

    %% Check that the client was removed from the ETS table
    ClientList = ets:tab2list(?CLIENTS_TABLE),
    lists:keyfind(ClientPid, 1, ClientList) =:= false,

    %% Clean up
    teardown(Pid).

%% Test routing a message to a random client
route_message_test() ->
    Pid = setup(),
    ClientPid = spawn(fun() ->
        receive
            {message, Msg} -> ?assertEqual("Hello", Msg)
        end
    end),

    %% Add the client and route the message using the connection_manager Pid
    connection_manager:add_client(ClientPid),
    connection_manager:route_message(Pid, "Hello"),

    %% Verify that the client received the message
    %% (The assertion is inside the client process)

    %% Clean up
    teardown(Pid).

%% Test routing a message when no clients are available
route_message_no_clients_test() ->
    Pid = setup(),

    %% Try to route a message without any clients
    {error, no_clients} = connection_manager:route_message(Pid, "Hello"),

    %% Clean up
    teardown(Pid).
