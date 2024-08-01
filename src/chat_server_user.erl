%%%-------------------------------------------------------------------
%% @doc chat_server user management.
%% @end
%%%-------------------------------------------------------------------

-module(chat_server_user).

-behaviour(gen_server).

%% API
-export([start_link/0, start_link/1, add_user/3, get_user/2]).

%% gen_server callbacks
-export([init/1, handle_call/3, handle_cast/2, handle_info/2, terminate/2, code_change/3]).

-define(SERVER, ?MODULE).

-record(state, {users = #{}}).

%% API Functions
start_link() ->
    start_link(?SERVER).

start_link(Name) ->
    gen_server:start_link({local, Name}, ?MODULE, [], []).

add_user(Name, Username, Password) ->
    gen_server:call(Name, {add_user, Username, Password}).

get_user(Name, Username) ->
    gen_server:call(Name, {get_user, Username}).

%% gen_server Callbacks
init([]) ->
    {ok, #state{}}.

handle_call({add_user, Username, Password}, _From, State) ->
    Users = State#state.users,
    case maps:is_key(Username, Users) of
        true -> {reply, {error, user_exists}, State};
        false ->
            NewUsers = maps:put(Username, {user, Username, Password}, Users),
            {reply, {ok, {user, Username, Password}}, State#state{users = NewUsers}}
    end;

handle_call({get_user, Username}, _From, State) ->
    Users = State#state.users,
    case maps:get(Username, Users, undefined) of
        undefined -> {reply, {error, user_not_found}, State};
        User -> {reply, {ok, User}, State}
    end;

handle_call(_Request, _From, State) ->
    {reply, {error, unknown_request}, State}.

handle_cast(_Msg, State) ->
    {noreply, State}.

handle_info(_Info, State) ->
    {noreply, State}.

terminate(_Reason, _State) ->
    ok.

code_change(_OldVsn, State, _Extra) ->
    {ok, State}.
