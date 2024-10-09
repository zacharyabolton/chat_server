%%%-------------------------------------------------------------------
%% @doc connection_manager_tests.erl
%% Tests for the connection manager
%% @end
%%%-------------------------------------------------------------------

-module(connection_manager_tests).

-include_lib("eunit/include/eunit.hrl").

%% Include the externalized state record
-include_lib("include/chat_state.hrl").

start_link_and_stop_test() ->
    {ok, Pid} = connection_manager:start_link(self()),
    ?assert(is_process_alive(Pid)),
    connection_manager:stop(Pid),
    ?assert(not is_process_alive(Pid)).

init_test() ->
    {ok, State} = connection_manager:init(self()),
    ?assertMatch(State, #state{client_pid=self()}).

