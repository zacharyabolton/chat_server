%%%-------------------------------------------------------------------
%% @doc chat_logic.erl
%% Contains logic for processing and routing chat messages.
%% Delegates client management and message routing to connection_manager.
%% @end
%%%-------------------------------------------------------------------

-module(chat_logic).
-export([process_message/2]).

process_message(SenderPid, Message) ->
    case connection_manager:route_message(SenderPid, Message) of
        {error, no_clients} ->
            io:format("No clients available to receive the message~n"),
            {error, no_clients};
        ok ->
            io:format("Message successfully routed~n"),
            ok
    end.
