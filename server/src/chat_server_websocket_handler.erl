%%%-------------------------------------------------------------------
%% @doc chat_server_websocket_handler.erl
%% WebSocket handler
%% @end
%%%-------------------------------------------------------------------

-module(chat_server_websocket_handler).
-behaviour(cowboy_websocket).

%% cowboy_websocket callbacks
-export([init/2, websocket_init/1, websocket_handle/2, websocket_info/2, terminate/3]).

init(Req, _Opts) ->
    %% Proceed with the WebSocket upgrade
    {cowboy_websocket, Req, #{}}.

websocket_init(State) ->
    %% This function is called in the WebSocket process
    ClientPid = self(),
    io:format("WebSocket connection established for ~p~n", [ClientPid]),

    %% Register the client in the connection_manager
    connection_manager:add_client(ClientPid),

    {ok, State}.

websocket_handle({text, Msg}, State) ->
    io:format("Received message: ~p~n", [Msg]),
    ClientPid = self(),
    chat_logic:process_message(ClientPid, Msg),
    {ok, State};
websocket_handle(_Frame, State) ->
    {ok, State}.

websocket_info({message, Message}, State) ->
    io:format("websocket_info called on client ~p with message: ~p~n", [self(), Message]),
    BinaryMessage = case is_binary(Message) of
        true -> Message;
        false -> unicode:characters_to_binary(Message)
    end,
    {reply, {text, BinaryMessage}, State};
websocket_info(_Info, State) ->
    {ok, State}.

terminate(_Reason, _Req, _State) ->
    ClientPid = self(),
    connection_manager:remove_client(ClientPid),
    io:format("WebSocket connection terminated for ~p~n", [ClientPid]),
    ok.
