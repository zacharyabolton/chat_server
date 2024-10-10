%%%-------------------------------------------------------------------
%% @doc chat_state.hrl
%% Common state record for chat-related modules
%% @end
%%%-------------------------------------------------------------------

-record(state, {
    client_pid :: pid() | undefined, %% Pid of the client process
    other_data  :: term() % Other necessary state information
}).
