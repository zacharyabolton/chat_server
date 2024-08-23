%%%-------------------------------------------------------------------
%% @doc router.erl
%% Cowboy router
%% @end
%%%-------------------------------------------------------------------

-module(router).

-export([dispatch/0]).

dispatch() ->
    cowboy_router:compile([
        {'_', [{"/", chat_server_websocket_handler, []}]}
    ]).
