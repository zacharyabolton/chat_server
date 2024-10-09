%%%-------------------------------------------------------------------
%% @doc chat_logic.erl
%% Chat server logic (GenServer)
%% @end
%%%-------------------------------------------------------------------

-module(chat_logic).
-export([process_message/1]).

%% Process and route message (simple pass-through)
process_message(Message) ->
    %% Here you would decide how to forward the message
    %% This could be sending to a randomly selected client
    io:format("Processing message: ~p~n", [Message]),
    %% For now, we just pass the message on to some recipient handler
    connection_manager:route_message(Message).
