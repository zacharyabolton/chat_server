-module(keep_alive).

%% Simple process that does nothing but keep the container alive

-export([start_link/0]).

start_link() ->
    Pid = spawn_link(fun() -> keep_alive_loop() end),
    {ok, Pid}.

keep_alive_loop() ->
    io:format("Keep alive sleeping on node ~p~n", [node()]),
  %% Sleep for 2 seconds
    timer:sleep(2000),
    keep_alive_loop().
