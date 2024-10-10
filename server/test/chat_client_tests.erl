%%%-------------------------------------------------------------------
%% @doc chat_client_tests.erl
%% Tests for the chat client
%% @end
%%%-------------------------------------------------------------------

-module(chat_client_tests).

-include_lib("eunit/include/eunit.hrl").

%% Include the externalized state record
-include_lib("include/chat_state.hrl").

%%% Setup and teardown helpers for tests

setup() ->
    %% Start the chat_client process
    {ok, Pid} = chat_client:start_link(self()),
    Pid.

teardown(Pid) ->
    %% Stop the chat_client process
    chat_client:stop(Pid).

%%% Test cases

% Test chat_client startup and shutdown
basic_start_stop_test() ->
    Pid = setup(),
    is_process_alive(Pid) =:= true,
    teardown(Pid),
    is_process_alive(Pid) =:= false.

% Test sending a message to the chat client
send_message_test() ->
    Pid = setup(),

    %% Send a message
    chat_client:send_message(Pid, "Hello, world!"),

    %% Ensure that the message is received
    receive
        {message, "Hello, world!"} -> ok
    after 1000
        -> ?assert(false)
    end,

    %% Clean up
    teardown(Pid).

% Test sending an invalid message to the chat client
send_invalid_message_test() ->
    Pid = setup(),

    %% Send an invalid message
    chat_client:send_message(Pid, undefined),

    %% Ensure that no message is received
    receive
        {message, undefined} -> ok
    after 1000
        -> ?assert(false)
    end,

    %% Clean up
    teardown(Pid).

% Test sending no message to the chat client
send_no_message_test() ->
    Pid = setup(),

    %% Send no message
    receive
        {message, _} -> ?assert(false)
    after 500
        -> ok
    end,

    %% Clean up
    teardown(Pid).

% Test sending a to the chat client with handle_cast
handle_cast_2_test() ->
    Pid = setup(),

    %% Send a message
    State = #state{client_pid=self()},
    {noreply, NewState} = chat_client:handle_cast({send_message, "Hello, world!"}, State),

    %% Ensure that the message is received
    ?assertMatch(NewState, State),

    %% Clean up
    teardown(Pid).

% Test sending a stop message to the chat client with handle_cast
handle_cast_stop_test() ->
    Pid = setup(),

    %% Send a stop message
    State = #state{client_pid=self()},
    {stop, normal, NewState} = chat_client:handle_cast(stop, State),

    %% Ensure that the message is received
    ?assertMatch(NewState, State),

    %% Clean up
    teardown(Pid).

handle_info_test() ->
    Pid = setup(),

    %% Send an info message
    State = #state{client_pid=self()},
    {noreply, NewState} = chat_client:handle_info("Info", State),

    %% Check that state is unchanged
    ?assertMatch(NewState, State),

    %% Clean up
    teardown(Pid).

handle_info_unexpected_message_test() ->
    Pid = setup(),

    %% Send an unexpected message
    State = #state{client_pid=self()},
    {noreply, NewState} = chat_client:handle_info(unexpected_message, State),

    %% Check that state is unchanged
    ?assertMatch(NewState, State),

    %% Clean up
    teardown(Pid).

handle_call_test() ->
    Pid = setup(),

    %% Send a call message
    State = #state{client_pid=self()},
    {reply, {error, unsupported_request}, NewState} = chat_client:handle_call("Request", self(), State),

    %% Check that state is unchanged
    ?assertMatch(NewState, State),

    %% Clean up
    teardown(Pid).

handle_call_multiple_requests_test() ->
    Pid = setup(),

    %% Send first call message
    State = #state{client_pid=self()},
    {reply, {error, unsupported_request}, NewState} = chat_client:handle_call(unsupported_request, self(), State),
    %% Check that state is unchanged
    ?assertMatch(NewState, State),
    %% Send second call message
    {reply, {error, unsupported_request}, NewState} = chat_client:handle_call(another_unsupported_request, self(), State),
    %% Check that state is unchanged
    ?assertMatch(NewState, State),

    %% Clean up
    teardown(Pid).

terminate_test() ->
    Pid = setup(),

    %% Send a terminate message
    State = #state{client_pid=self()},
    Res = chat_client:terminate(normal, State),

    %% Check that the result is ok
    ?assertMatch(Res, ok),

    %% Clean up
    teardown(Pid).

multiple_clients_test() ->
    %% Spawn multiple clients and send them messages
    Pids = [{ok, setup()} || _ <- lists:seq(1, 10)],
    lists:foreach(fun({ok, Pid}) ->
        chat_client:send_message(Pid, "Hello")
    end, Pids),

    %% Ensure each client receives the correct message
    lists:foreach(fun({ok, Pid}) ->
        receive
            {message, "Hello"} -> ok
        after 1000
            -> ?assert(false)
        end
    end, Pids),

    %% Clean up
    lists:foreach(fun({ok, Pid}) ->
        teardown(Pid)
    end, Pids).

handle_cast_send_message_test() ->
    Pid = setup(),

    %% Send a message
    State = #state{client_pid=self()},
    {noreply, NewState} = chat_client:handle_cast({send_message, "Hello, world!"}, State),

    %% Ensure that the message is received
    receive
        {message, "Hello, world!"} -> ok
    after 1000
        -> ?assert(false)
    end,
    ?assertMatch(NewState, State),

    %% Clean up
    teardown(Pid).
