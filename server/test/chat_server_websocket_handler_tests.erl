%%%-------------------------------------------------------------------
%% @doc chat_server_websocket_handler_tests.erl
%% Tests for the chat_server_websocket_handler
%% @end
%%%-------------------------------------------------------------------

-module(chat_server_websocket_handler_tests).
-include_lib("eunit/include/eunit.hrl").

%%% Setup and teardown helpers for tests

setup() ->
    meck:new(connection_manager, [passthrough]),
    meck:new(chat_logic, [passthrough]),
    ok.

teardown() ->
    meck:unload(connection_manager),
    meck:unload(chat_logic),
    ok.

%%% Test cases

% chat_server_websocket_handler_test_() ->
%     {foreach,
%      fun setup/0,
%      fun teardown/1,
%      [
%       fun test_init/1,
%       fun test_websocket_handle_text/1,
%       fun test_websocket_handle_other/1,
%       fun test_websocket_info/1,
%       fun test_terminate/1
%      ]}.

init_test() ->
    setup(),

    Req = #{},
    Result = chat_server_websocket_handler:init(Req, []),
    ?assertMatch({cowboy_websocket, Req, #{}}, Result),

    teardown().

websocket_init_test() ->
    setup(),

    meck:expect(connection_manager, add_client, fun(_) -> ok end),
    State = #{},
    Result = chat_server_websocket_handler:websocket_init(State),
    ?assertMatch({cowboy_websocket, Req, #{}}, Result),
    ?assert(meck:called(connection_manager, add_client, '_')),

    teardown().

% test_websocket_handle_text(_) ->
%     meck:expect(chat_logic, process_message, fun(_) -> ok end),
%     Message = <<"Hello, World!">>,
%     Result = ?MOD:websocket_handle({text, Message}, state),
%     [
%      ?_assertMatch({ok, state}, Result),
%      ?_assert(meck:called(chat_logic, process_message, [Message]))
%     ].

% test_websocket_handle_other(_) ->
%     Result = ?MOD:websocket_handle({binary, <<1,2,3>>}, state),
%     [
%      ?_assertMatch({ok, state}, Result)
%     ].

% test_websocket_info(_) ->
%     Info = {internal_msg, "test"},
%     Result = ?MOD:websocket_info(Info, state),
%     [
%      ?_assertMatch({ok, state}, Result)
%     ].

% test_terminate(_) ->
%     meck:expect(connection_manager, remove_client, fun(_) -> ok end),
%     Reason = normal,
%     Req = #{},
%     State = #{client_pid => self()},
%     ?MOD:terminate(Reason, Req, State),
%     [
%      ?_assert(meck:called(connection_manager, remove_client, '_'))
%     ].
