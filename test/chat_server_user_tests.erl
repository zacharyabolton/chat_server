%%%-------------------------------------------------------------------
%% @doc chat_server user management tests.
%% @end
%%%-------------------------------------------------------------------

-module(chat_server_user_tests).

-include_lib("eunit/include/eunit.hrl").

user_registration_test() ->
    {ok, _} = chat_server_user:start_link(),
    ?assertEqual(
       {ok, {user, "username", "password"}},
       chat_server_user:add_user("username", "password")),
    ?assertEqual(
       {error, user_exists},
       chat_server_user:add_user("username", "password")).

user_retrieval_test() ->
    {ok, _} = chat_server_user:start_link(),
    chat_server_user:add_user("username", "password"),
    ?assertEqual(
       {ok, {user, "username", "password"}},
       chat_server_user:get_user("username")),
    ?assertEqual(
       {error, user_not_found},
       chat_server_user:get_user("nonexistent")).
