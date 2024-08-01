%%%-------------------------------------------------------------------
%% @doc chat_server user management tests.
%% @end
%%%-------------------------------------------------------------------

-module(chat_server_user_tests).

-include_lib("eunit/include/eunit.hrl").

setup(Name) ->
    {ok, Pid} = chat_server_user:start_link(Name),
    Pid.

teardown(Pid) ->
    gen_server:stop(Pid).

%% Test cases

user_registration_test() ->
    Name = chat_server_user_test_1,
    Pid = setup(Name),
    ?assertEqual({ok, {user, "username", "password"}}, chat_server_user:add_user(Name, "username", "password")),
    ?assertEqual({error, user_exists}, chat_server_user:add_user(Name, "username", "password")),
    teardown(Pid).

user_retrieval_test() ->
    Name = chat_server_user_test_2,
    Pid = setup(Name),
    chat_server_user:add_user(Name, "username", "password"),
    ?assertEqual({ok, {user, "username", "password"}}, chat_server_user:get_user(Name, "username")),
    ?assertEqual({error, user_not_found}, chat_server_user:get_user(Name, "nonexistent")),
    teardown(Pid).
