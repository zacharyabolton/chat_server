%%%-------------------------------------------------------------------
%% @doc chat_server_websocket_handler.erl
%% WebSocket handler
%% @end
%%%-------------------------------------------------------------------

-module(chat_server_websocket_handler).
-behaviour(cowboy_websocket).

%% Public API
-export([start_link/0, stop/1]).

%% cowboy_websocket callbacks (required for the behavior)
-export([init/2, websocket_handle/2, websocket_info/2, terminate/3]).

%%%-------------------------------------------------------------------
%%% Public API
%%%-------------------------------------------------------------------

%%%-------------------------------------------------------------------
%% @doc Starts a new chat_server_websocket_handler process linked to the caller.
%% The process handles communication with a specific WebSocket client.
%% @param Req - The cowboy request object.
%% @end
%%%-------------------------------------------------------------------
start_link() ->
    {ok, self()}.

%%%-------------------------------------------------------------------
%% @doc Stops the chat_server_websocket_handler process by sending a stop message.
%% @param Pid - The Pid of the chat_server_websocket_handler process to stop.
%% @end
%%%-------------------------------------------------------------------
stop(Pid) ->
    gen_server:cast(Pid, stop).


%%%-------------------------------------------------------------------
%%% gen_server Callbacks
%%%-------------------------------------------------------------------

%%%-------------------------------------------------------------------
%% @doc Initializes the chat_server_websocket_handler process state.
%% Called when the process starts.
%% @param Req - The cowboy request object.
%% @param Opts - The options passed to the WebSocket handler.
%% @end
%%%-------------------------------------------------------------------
init(Req, _Opts) ->
    %% Register the client when WebSocket connection is established
    ClientPid = self(),
    io:format("WebSocket connection established for ~p~n", [ClientPid]),

    %% Register the client in the connection_manager
    connection_manager:add_client(ClientPid),

    %% Proceed with the WebSocket upgrade
    {cowboy_websocket, Req, #{client_pid => ClientPid}}.

%%%-------------------------------------------------------------------
%% @doc Handles WebSocket messages received from the client.
%% @param Frame - The WebSocket frame received.
%% @param State - The current state of the WebSocket handler.
%% @end
%%%-------------------------------------------------------------------
websocket_handle({text, Msg}, State) ->
    io:format("Received message: ~p~n", [Msg]),

    %% Forward the message to the chat_logic for processing
    chat_logic:process_message(Msg),

    %% No state modification needed
    {ok, State};
websocket_handle(_Frame, State) ->
    %% Handle other WebSocket frames (binary, ping, etc.)
    {ok, State}.

%%%-------------------------------------------------------------------
%% @doc Handles WebSocket information messages.
%% @param Info - The WebSocket information message.
%% @param State - The current state of the WebSocket handler.
%% @end
%%%-------------------------------------------------------------------
websocket_info(_Info, State) ->
    %% Handle WebSocket information messages (e.g., ping, pong)
    io:format("Received WebSocket info: ~p~n", [_Info]),
    {ok, State}.

%%%-------------------------------------------------------------------
%% @doc Terminates the WebSocket handler process.
%% @param Reason - The reason for termination.
%% @param Req - The cowboy request object.
%% @param State - The current state of the WebSocket handler.
%% @end
%%%-------------------------------------------------------------------
terminate(_Reason, _Req, #{client_pid := ClientPid}) ->
    %% Clean up: remove the client from connection_manager
    connection_manager:remove_client(ClientPid),

    io:format("WebSocket connection terminated for ~p~n", [ClientPid]),
    ok.
