%%%-------------------------------------------------------------------
%% @doc chat_server server tests.
%% @end
%%%-------------------------------------------------------------------

-module(chat_server_tests).

-include_lib("eunit/include/eunit.hrl").

%% Test cases

server_running_test() ->
  {ok, Pid} = chat_server:start_link(),
  ?assert(is_process_alive(Pid)),
  chat_server:stop().

server_stopping_test() ->
  {ok, Pid} = chat_server:start_link(),
  chat_server:stop(),
  ?assert(not is_process_alive(Pid)).

add_user_test() ->
  chat_server:start_link(),
  NewUserName = "John Doe",
  NewUser = chat_server:add_user(NewUserName),
  ?assertEqual({ok, {user, NewUserName}},
               NewUser),
  chat_server:stop().

get_user_test() ->
  chat_server:start_link(),
  NewUserName = "John Doe",
  chat_server:add_user(NewUserName),
  User = chat_server:get_user(NewUserName),
  ?assertEqual({ok, {user, NewUserName}},
               User),
  chat_server:stop().

remove_user_test() ->
  chat_server:start_link(),
  NewUserName = "John Doe",
  chat_server:add_user(NewUserName),
  chat_server:remove_user(NewUserName),
  User = chat_server:get_user(NewUserName),
  ?assertEqual({error, user_not_found},
               User),
  chat_server:stop().
