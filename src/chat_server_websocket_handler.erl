%%%-------------------------------------------------------------------
%% @doc chat_server_websocket_handler.erl
%% WebSocket handler
%% @end
%%%-------------------------------------------------------------------

-module(chat_server_websocket_handler).
-behaviour(cowboy_websocket).

%% Required callback functions
-export([init/2, websocket_handle/2, websocket_info/2, terminate/3]).

%% Initialize the WebSocket connection
init(Req, _Opts) ->
    {cowboy_websocket, Req, undefined_state}.

%% Handle incoming WebSocket frames (minimal implementation)
websocket_handle(_Frame, State) ->
    {ok, State}.

%% Handle other messages (minimal implementation)
websocket_info(_Info, State) ->
    {ok, State}.

%% Terminate the WebSocket connection
terminate(_Reason, _Req, _State) ->
    ok.
