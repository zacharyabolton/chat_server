%%%-------------------------------------------------------------------
%% @doc connection_manager.erl
%% Manages connections and pairing
%% @end
%%%-------------------------------------------------------------------

-module(connection_manager).
-behaviour(gen_server).

%% Public API
-export([start_link/1, stop/1]).

%% gen_server callbacks (required for the behavior)
-export([init/1, add_client/1, remove_client/1, route_message/1, handle_cast/2,
         handle_call/3, terminate/2]).

%% Include the externalized state record
-include_lib("include/chat_state.hrl").

%% In-memory client registry (for active WebSocket connections)
-define(CLIENTS_TABLE, clients).

%%%-------------------------------------------------------------------
%%% Public API
%%%-------------------------------------------------------------------

%%%-------------------------------------------------------------------
%% @doc Starts a new connection_manager process linked to the caller.
%% Initializes the ETS table for managing active WebSocket clients.
%% This process will handle client registrations and message routing.
%% @param Pid - The process identifier (Pid) of the connection_manager process.
%% @end
%%%-------------------------------------------------------------------
start_link(Pid) ->
    gen_server:start_link(?MODULE, Pid, []).

%%%-------------------------------------------------------------------
%% @doc Stops the connection_manager process by sending a stop message.
%% @param Pid - The Pid of the connection_manager process to stop.
%% @end
%%%-------------------------------------------------------------------
stop(Pid) ->
    gen_server:cast(Pid, stop).

%%%-------------------------------------------------------------------
%%% gen_server Callbacks
%%%-------------------------------------------------------------------

%%%-------------------------------------------------------------------
%% @doc Initializes the connection manager process.
%% Creates an ETS table to track connected clients.
%% Called when the process starts.
%% @param Args - List of arguments, currently unused.
%% @end
%%%-------------------------------------------------------------------
init(Pid) ->
    ets:new(?CLIENTS_TABLE, [set, named_table, public]),
    {ok, #state{client_pid=Pid}}.

%%%-------------------------------------------------------------------
%% @doc Asynchronously adds a client to the ETS table.
%% Each client is associated with a timestamp using monotonic time.
%% @param ClientPid - The process identifier (Pid) of the WebSocket client.
%% @end
%%%-------------------------------------------------------------------
add_client(ClientPid) ->
    gen_server:cast(self(), {add_client, ClientPid}).

%%%-------------------------------------------------------------------
%% @doc Asynchronously removes a client from the ETS table.
%% Removes the client by its process identifier (Pid).
%% @param ClientPid - The Pid of the WebSocket client to remove.
%% @end
%%%-------------------------------------------------------------------
remove_client(ClientPid) ->
    gen_server:cast(self(), {remove_client, ClientPid}).

%%%-------------------------------------------------------------------
%% @doc Synchronously routes a message to a random connected client.
%% Looks up all active clients from the ETS table, selects one at random,
%% and forwards the message to that client.
%% @param Message - The message to route to a random client.
%% @end
%%%-------------------------------------------------------------------
route_message(Message) ->
    gen_server:call(self(), {route_message, Message}).

%%%-------------------------------------------------------------------
%% @doc Handles the `add_client`, `remove_client`, and `stop` asynchronous
%% messages.
%% - `add_client`: Inserts the client into the ETS table with a monotonic
%% timestamp.
%% - `remove_client`: Removes the client from the ETS table.
%% - `stop`: Stops the process.
%% @param Msg - The incoming cast message, either `{add_client, ClientPid}`,
%% `{remove_client, ClientPid}`, or `stop`.
%% @param State - The current state of the process.
%% @end
%%%-------------------------------------------------------------------
handle_cast({add_client, ClientPid}, State) ->
    %% Use monotonic time for tracking purposes
    Timestamp = erlang:monotonic_time(),
    ets:insert(?CLIENTS_TABLE, {ClientPid, Timestamp}),
    {noreply, State};
handle_cast({remove_client, ClientPid}, State) ->
    ets:delete(?CLIENTS_TABLE, ClientPid),
    {noreply, State};
handle_cast(stop, State) ->
    {stop, normal, State}.

%%%-------------------------------------------------------------------
%% @doc Handles the `route_message` synchronous request.
%% Looks up all active clients in the ETS table, selects a random client,
%% and forwards the message to the selected client.
%% If no clients are available, returns an error.
%% @param Request - The `{route_message, Message}` request.
%% @param From - Information about the caller.
%% @param State - The current state of the process.
%% @end
%%%-------------------------------------------------------------------
handle_call({route_message, Message}, _From, State) ->
    ClientPids = ets:tab2list(?CLIENTS_TABLE),
    case ClientPids of
        [] ->
            io:format("No clients to route message~n"),
            {reply, {error, no_clients}, State};
        _ ->
            {ClientPid, _} = lists:nth(rand:uniform(length(ClientPids)), ClientPids),
            chat_client:send_message(ClientPid, Message),
            {reply, ok, State}
    end.

%%%-------------------------------------------------------------------
%% @doc Handles process termination.
%% Called when the process is stopped, either manually via stop/0
%% or by the supervisor. Clean up ETS tables or other resources here.
%% @param Reason - Reason for termination.
%% @param State - The current state of the process.
%% @end
%%%-------------------------------------------------------------------
terminate(_Reason, _State) ->
    %% Example: Delete the ETS table if it's no longer needed
    ets:delete(?CLIENTS_TABLE),
    ok.
