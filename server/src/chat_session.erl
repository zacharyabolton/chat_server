%%%-------------------------------------------------------------------
%% @doc chat_session.erl
%% Manages individual chat sessions
%% @end
%%%-------------------------------------------------------------------

-module(chat_session).
-export([start_session/1, stop_session/1, add_client/2, remove_client/2]).

%%%-------------------------------------------------------------------
%%% Public API
%%%-------------------------------------------------------------------

%%%-------------------------------------------------------------------
%% @doc Starts a new chat session.
%% Initializes the session state with a unique identifier.
%% @param SessionId - The unique identifier for the session (atom).
%% @end
%%%-------------------------------------------------------------------
start_session(SessionId) when is_atom(SessionId) ->
    io:format("Starting session: ~p~n", [SessionId]),
    %% Keep track of session data in memory (e.g., active clients)
    ets:new(SessionId, [set, named_table, public]),
    {ok, SessionId}.

%%%-------------------------------------------------------------------
%% @doc Stops the chat session.
%% Removes the session data from memory.
%% @param SessionId - The unique identifier for the session (atom).
%% @end
%%%-------------------------------------------------------------------
stop_session(SessionId) when is_atom(SessionId) ->
    io:format("Stopping session: ~p~n", [SessionId]),
    ets:delete(SessionId),
    ok.

%%%-------------------------------------------------------------------
%% @doc Adds a client to the chat session.
%% @param SessionId - The unique identifier for the session (atom).
%% @param ClientPid - The process identifier (Pid) of the client.
%% @end
%%%-------------------------------------------------------------------
add_client(SessionId, ClientPid) when is_atom(SessionId), is_pid(ClientPid) ->
    %% Use monotonic time for tracking purposes
    Timestamp = erlang:monotonic_time(),
    ets:insert(SessionId, {ClientPid, Timestamp}),
    io:format("Client ~p added to session ~p~n", [ClientPid, SessionId]).

%%%-------------------------------------------------------------------
%% @doc Removes a client from the chat session.
%% @param SessionId - The unique identifier for the session (atom).
%% @param ClientPid - The process identifier (Pid) of the client.
%% @end
%%%-------------------------------------------------------------------
remove_client(SessionId, ClientPid) when is_atom(SessionId), is_pid(ClientPid) ->
    ets:delete(SessionId, ClientPid),
    io:format("Client ~p removed from session ~p~n", [ClientPid, SessionId]).
