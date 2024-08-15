%%%-------------------------------------------------------------------
%% @doc chat_server server tests.
%% @end
%%%-------------------------------------------------------------------

-module(chat_server_tests).

-include_lib("eunit/include/eunit.hrl").

%% Test cases

dummy_test() ->
  io:format("Dummy test~n"),
  ?assert(false).

% top_lvl_sup_alive_test() ->
%   ok = application:start(chat_server),
%   ?assert(is_process_alive(whereis(chat_server_sup))),
%   ok = application:stop(chat_server).
%
% chat_connection_sup_alive_test() ->
%   ok = application:start(chat_server),
%   ?assert(is_process_alive(whereis(chat_connection_sup))),
%   ok = application:stop(chat_server).
%
% chat_user_registry_alive_test() ->
%   ok = application:start(chat_server),
%   ?assert(is_process_alive(whereis(chat_user_registry))),
%   ok = application:stop(chat_server).

% chat_connection_handler_alive_test() ->
%   ok = application:start(chat_server),
%   ?assert(is_process_alive(whereis(chat_connection_handler))),
%   ok = application:stop(chat_server).

% server_running_test() ->
%   ok = application:start(chat_server),
%   Pid = whereis(chat_server_sup),
%   ?assert(is_process_alive(Pid)),
%   ok = application:stop(chat_server),
%   wait_for_shutdown(Pid, 10000).
%
% server_stopping_test() ->
%   ok = application:start(chat_server),
%   Pid = whereis(chat_server_sup),
%   ok = application:stop(chat_server),
%   wait_for_shutdown(Pid, 10000),  % 10 seconds timeout
%   ?assert(not is_process_alive(Pid)).
%
% add_user_test() ->
%   ok = application:start(chat_server),
%   Pid = whereis(chat_server_sup),
%   {ok, ClientPid} = chat_server_sup:start_client(chat_server, john_doe),
%   % User = chat_server:get_user(john_doe),
%   % ?assertEqual({ok, {user, john_doe}}, User),
%   ok = application:stop(chat_server),
%   wait_for_shutdown(Pid, 10000).

% get_user_test() ->
%   {ok, _} = chat_server_sup:start_link(),
%   {ok, Pid} = chat_server_sup:start_client(node(), john_doe),
%   User = chat_server:get_user(john_doe),
%   ?assertEqual({ok, {user, john_doe}},
%                User),
%   chat_server:stop().
%
% remove_user_test() ->
%   {ok, _} = chat_server_sup:start_link(),
%   {ok, Pid} = chat_server_sup:start_client(node(), john_doe),
%   chat_server:remove_user(john_doe),
%   User = chat_server:get_user(john_doe),
%   ?assertEqual({error, user_not_found},
%                User),
%   chat_server:stop().
%
% transfer_message_between_users_test() ->
%   {ok, _} = chat_server_sup:start_link(),
%   {ok, Pid1} = chat_server_sup:start_client(node(), john_doe),
%   {ok, Pid2} = chat_server_sup:start_client(node(), "Jane Doe"),
%   Message = "Hello, Jane!",
%   ServerReply = chat_server:transfer_message(john_doe, "Jane Doe", Message),
%   ?assertEqual({ok, {message, sent}}, ServerReply),
%   chat_server:stop().

wait_for_shutdown(Pid, Timeout) ->
  Ref = erlang:monitor(process, Pid),
  receive
    {'DOWN', Ref, process, Pid, _Reason} ->
      ok
  after Timeout ->
      exit(timeout)
  end.
