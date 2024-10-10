%%%-------------------------------------------------------------------
%% @doc chat_logic.erl
%% Contains logic for processing and routing chat messages.
%% Delegates client management and message routing to connection_manager.
%% @end
%%%-------------------------------------------------------------------

-module(chat_logic).
-export([process_message/1, register_client/1, deregister_client/1]).

%%%-------------------------------------------------------------------
%%% Public API
%%%-------------------------------------------------------------------

%%%-------------------------------------------------------------------
%% @doc Processes a message to be routed.
%% Delegates message routing to the connection_manager module.
%% Handles potential errors if no clients are available.
%% @param Message - The message to be routed to a random client.
%% @end
%%%-------------------------------------------------------------------
process_message(Message) ->
    case connection_manager:route_message(self(), Message) of
        {error, no_clients} ->
            io:format("No clients available to receive the message~n"),
            {error, no_clients};
        ok ->
            io:format("Message successfully routed~n"),
            ok
    end.

%%%-------------------------------------------------------------------
%% @doc Registers a client by delegating to the connection_manager module.
%% This adds the client's Pid to the in-memory ETS table in connection_manager.
%% @param ClientPid - The process identifier (Pid) of the WebSocket client to register.
%% @end
%%%-------------------------------------------------------------------
register_client(ClientPid) ->
    connection_manager:add_client(ClientPid).

%%%-------------------------------------------------------------------
%% @doc Deregisters a client by delegating to the connection_manager module.
%% This removes the client's Pid from the in-memory ETS table in connection_manager.
%% @param ClientPid - The process identifier (Pid) of the WebSocket client to deregister.
%% @end
%%%-------------------------------------------------------------------
deregister_client(ClientPid) ->
    connection_manager:remove_client(ClientPid).
