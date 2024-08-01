%%%-------------------------------------------------------------------
%% @doc chat_server server.
%% @end
%%%-------------------------------------------------------------------

-module(chat_server).

-behaviour(gen_server).

-include("chat_server.hrl").

%% API
-export([start_link/0, add_user/1, get_user/1, remove_user/1, stop/0]).

%% gen_server callbacks
-export([init/1,
         handle_call/3,
         handle_cast/2,
         handle_info/2,
         terminate/2,
         code_change/3]).

%% API Functions

start_link() ->
  gen_server:start_link({local, ?MODULE}, ?MODULE, [], []).

add_user(Username) ->
  gen_server:call(?MODULE, {user, Username}).

get_user(Username) ->
  gen_server:call(?MODULE, {get_user, Username}).

remove_user(Username) ->
  gen_server:call(?MODULE, {remove_user, Username}).

stop() ->
  gen_server:call(?MODULE, stop).

%% gen_server Callbacks

init([]) ->
  {ok, #state{}}.

handle_call({user, Username}, _From, State) ->
  Users = State#state.users,
  case maps:is_key(Username, Users) of
    true -> {reply, {error, user_exists}, State};
    false ->
      NewUsers = maps:put(Username, {user, Username}, Users),
      {reply, {ok, {user, Username}}, State#state{users = NewUsers}}
  end;
handle_call({get_user, Username}, _From, State) ->
  Users = State#state.users,
  case maps:get(Username, Users, undefined) of
    undefined -> {reply, {error, user_not_found}, State};
    User -> {reply, {ok, User}, State}
  end;
handle_call({remove_user, Username}, _From, State) ->
  Users = State#state.users,
  case maps:is_key(Username, Users) of
    true ->
      NewUsers = maps:remove(Username, Users),
      {reply, ok, State#state{users = NewUsers}};
    false -> {reply, {error, user_not_found}, State}
  end;
handle_call(stop, _From, State) ->
  {stop, normal, ok, State};
handle_call(_Request, _From, State) ->
  {reply, ok, State}.

handle_cast(_Msg, State) ->
  {noreply, State}.

handle_info(_Info, State) ->
  {noreply, State}.

terminate(_Reason, _State) ->
  ok.

code_change(_OldVsn, State, _Extra) ->
  {ok, State}.
