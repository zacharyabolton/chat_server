%%%-------------------------------------------------------------------
%% @doc chat_server public API
%% @end
%%%-------------------------------------------------------------------

-module(chat_server_app).
-behaviour(application).
%% Application callbacks
-export([start/2, stop/1]).

%%====================================================================
%% API
%%====================================================================

start(_Type, _Args) ->
    io:format("Starting chat_server_app~n"),
    Dispatch = cowboy_router:compile([
        {'_', [{"/", hello_handler, []}]}
    ]),
    {ok, _} = cowboy:start_clear(my_http_listener,
        [{port, 8080}],
        #{env => #{dispatch => Dispatch}}
    ),
    chat_server_sup:start_link().

stop(_State) ->
	ok.
