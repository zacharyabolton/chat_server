%%%-------------------------------------------------------------------
%% @doc chat_client.erl
%% Client-side logic
%% @end
%%%-------------------------------------------------------------------

-module(chat_client).
-behaviour(gen_server).

%% Public API
-export([start_link/1, stop/1, send_message/2]).

%% gen_server callbacks (required for the behavior)
-export([init/1, handle_cast/2, handle_call/3, handle_info/2, terminate/2]).

%% Include the externalized state record
-include_lib("include/chat_state.hrl").

%%%-------------------------------------------------------------------
%%% Public API
%%%-------------------------------------------------------------------

%%%-------------------------------------------------------------------
%% @doc Starts a new chat_client process linked to the caller.
%% The process handles communication with a specific WebSocket client.
%% @param ClientPid - The process identifier (Pid) of the WebSocket client.
%% @end
%%%-------------------------------------------------------------------
start_link(ClientPid) ->
    gen_server:start_link(?MODULE, ClientPid, []).

%%%-------------------------------------------------------------------
%% @doc Stops the chat_client process by sending a stop message.
%% @param Pid - The Pid of the chat_client process to stop.
%% @end
%%%-------------------------------------------------------------------
stop(Pid) ->
    gen_server:cast(Pid, stop).

%%%-------------------------------------------------------------------
%% @doc Sends a message to the client process for delivery to the WebSocket.
%% @param ClientPid - The Pid of the target chat client.
%% @param Message - The message to send to the client.
%% @end
%%%-------------------------------------------------------------------
send_message(ClientPid, Message) ->
    gen_server:cast(ClientPid, {send_message, Message}).


%%%-------------------------------------------------------------------
%%% gen_server Callbacks
%%%-------------------------------------------------------------------

%%%-------------------------------------------------------------------
%% @doc Initializes the chat_client process state with the given ClientPid.
%% Called when the process starts.
%% @param ClientPid - The process identifier of the WebSocket client.
%% @end
%%%-------------------------------------------------------------------
init(ClientPid) ->
    {ok, #state{client_pid=ClientPid}}.

%%%-------------------------------------------------------------------
%% @doc Handles asynchronous messages such as `send_message` and `stop`.
%% For `send_message`, the message is delivered to the WebSocket client.
%% For `stop`, the process is terminated.
%% @param Msg - The incoming cast message.
%% @param State - The current state of the process.
%% @end
%%%-------------------------------------------------------------------
handle_cast({send_message, Message}, State) ->
    %% Send message to the client
    io:format("Sending message to client: ~p~n", [Message]),
    State#state.client_pid ! {message, Message},
    {noreply, State};
handle_cast(stop, State) ->
    {stop, normal, State}.

%%%-------------------------------------------------------------------
%% @doc Handles system or other unexpected messages.
%% Currently logs and ignores unexpected messages.
%% @param Info - The message received.
%% @param State - The current state of the process.
%% @end
%%%-------------------------------------------------------------------
handle_info(Info, State) ->
    io:format("Received unexpected message: ~p~n", [Info]),
    {noreply, State}.

%%%-------------------------------------------------------------------
%% @doc Handles synchronous calls (gen_server:call).
%% Returns an error indicating that no synchronous requests are supported.
%% @param Request - The request sent by gen_server:call.
%% @param From - The caller information.
%% @param State - The current state of the process.
%% @end
%%%-------------------------------------------------------------------
handle_call(_Request, _From, State) ->
    {reply, {error, unsupported_request}, State}.

%%%-------------------------------------------------------------------
%% @doc Handles process termination and cleanup.
%% Called when the process is stopping.
%% @param Reason - The reason for termination.
%% @param State - The current state of the process.
%% @end
%%%-------------------------------------------------------------------
terminate(_Reason, _State) ->
    ok.
