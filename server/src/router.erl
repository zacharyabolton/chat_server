%%%-------------------------------------------------------------------
%% @doc router.erl
%% Cowboy router
%% @end
%%%-------------------------------------------------------------------

-module(router).

-export([dispatch/0]).

dispatch() ->
  cowboy_router:compile([
    {'_', [

    %% WebSocket handler route
    {"/ws", chat_server_websocket_handler, []},

    %% Serve static files from the 'static' directory in your 'priv' directory
    {"/static/[...]", cowboy_static, {priv_dir, chat_server, "static/static"}},

    %% Serve 'index.html' for any other request (client-side routing support)
    {"/[...]", cowboy_static, {priv_file, chat_server, "static/index.html"}}
  ]}
  ]).
