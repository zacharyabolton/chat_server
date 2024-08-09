-module(keep_alive).

%% Simple process that does nothing but keep the container alive

-export([start_link/0]).

start_link() ->
    keep_alive().

keep_alive() ->
  io:format("Keep alive sleeping on node~p~n", [node()]),
  %% Sleep for 2 seconds
  timer:sleep(2000),
  keep_alive().
