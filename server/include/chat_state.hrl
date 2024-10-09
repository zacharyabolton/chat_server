%%%-------------------------------------------------------------------
%% @doc chat_state.hrl
%% Common state record for chat-related modules
%% @end
%%%-------------------------------------------------------------------

-record(state, {
    client_pid  :: pid(), % The WebSocket client's process identifier
    other_data  :: term() % Other necessary state information
}).
