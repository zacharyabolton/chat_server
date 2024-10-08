%%%-------------------------------------------------------------------
%% @doc chat_client_tests.erl
%% Tests for the chat client
%% @end
%%%-------------------------------------------------------------------

-module(chat_client_tests).

-include_lib("eunit/include/eunit.hrl").

%% Client state
-record(state, {client_pid}).

start_link_test() ->
    ?assertMatch({ok, _Pid}, chat_client:start_link(self())).

stop_test() ->
    {ok, Pid} = chat_client:start_link(self()),
    chat_client:stop(Pid),
    ?assert(not is_process_alive(Pid)).

send_message_test() ->
    {ok, Pid} = chat_client:start_link(self()),
    chat_client:send_message(Pid, "Hello, world!"),
    receive
        {message, "Hello, world!"} -> ok
    after 1000
        -> ?assert(false)
    end.

send_invalid_message_test() ->
    {ok, Pid} = chat_client:start_link(self()),
    chat_client:send_message(Pid, undefined),
    receive
        {message, undefined} -> ok
    after 1000
        -> ?assert(false)
    end.

send_no_message_test() ->
    {ok, Pid} = chat_client:start_link(self()),
    receive
        {message, _} -> ?assert(false)
    after 500
        -> ok
    end.

init_test() ->
    {ok, State} = chat_client:init(self()),
    IncomingPid = State#state.client_pid,
    TestPid = self(),
    ?assertMatch(IncomingPid, TestPid).

handle_cast_2_test() ->
    {ok, _} = chat_client:start_link(self()),
    State = #state{client_pid=self()},
    {noreply, NewState} = chat_client:handle_cast({send_message, "Hello, world!"}, State),
    ?assertMatch(NewState, State).

handle_cast_stop_test() ->
    {ok, _} = chat_client:start_link(self()),
    State = #state{client_pid=self()},
    {stop, normal, NewState} = chat_client:handle_cast(stop, State),
    ?assertMatch(NewState, State).

handle_info_test() ->
    {ok, _} = chat_client:start_link(self()),
    State = #state{client_pid=self()},
    {noreply, NewState} = chat_client:handle_info("Info", State),
    ?assertMatch(NewState, State).

handle_info_unexpected_message_test() ->
    {ok, _} = chat_client:start_link(self()),
    State = #state{client_pid=self()},
    {noreply, NewState} = chat_client:handle_info(unexpected_message, State),
    %% Check that state is unchanged
    ?assertMatch(NewState, State).

handle_call_test() ->
    {ok, _} = chat_client:start_link(self()),
    State = #state{client_pid=self()},
    {reply, {error, unsupported_request}, NewState} = chat_client:handle_call("Request", self(), State),
    ?assertMatch(NewState, State).

handle_call_multiple_requests_test() ->
    {ok, _} = chat_client:start_link(self()),
    State = #state{client_pid=self()},
    {reply, {error, unsupported_request}, NewState} = chat_client:handle_call(unsupported_request, self(), State),
    ?assertMatch(NewState, State),
    {reply, {error, unsupported_request}, NewState} = chat_client:handle_call(another_unsupported_request, self(), State),
    ?assertMatch(NewState, State).

terminate_test() ->
    {ok, _} = chat_client:start_link(self()),
    State = #state{client_pid=self()},
    Res = chat_client:terminate(normal, State),
    ?assertMatch(Res, ok).

multiple_clients_test() ->
    %% Spawn multiple clients and send them messages
    Pids = [chat_client:start_link(self()) || _ <- lists:seq(1, 10)],
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
    end, Pids).

handle_cast_send_message_test() ->
    {ok, Pid} = chat_client:start_link(self()),
    State = #state{client_pid=self()},
    {noreply, NewState} = chat_client:handle_cast({send_message, "Hello, world!"}, State),
    receive
        {message, "Hello, world!"} -> ok
    after 1000
        -> ?assert(false)
    end,
    ?assertMatch(NewState, State).
