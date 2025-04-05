%%%-------------------------------------------------------------------
%% @doc connection_manager.erl
%% Manages connections and pairing
%% @end
%%%-------------------------------------------------------------------

-module(connection_manager).
-behaviour(gen_server).

%% Public API
-export([start_link/0, add_client/1, remove_client/1, route_message/2]).

%% gen_server callbacks
-export([init/1, handle_call/3, handle_cast/2, handle_info/2, terminate/2]).

-define(CLIENTS_TABLE, clients).

start_link() ->
    gen_server:start_link({local, ?MODULE}, ?MODULE, [], []).

init([]) ->
    ets:new(?CLIENTS_TABLE, [set, named_table, public]),
    {ok, #{}}.

add_client(ClientPid) ->
    gen_server:cast(?MODULE, {add_client, ClientPid}).

remove_client(ClientPid) ->
    gen_server:cast(?MODULE, {remove_client, ClientPid}).

route_message(SenderPid, Message) ->
    gen_server:call(?MODULE, {route_message, SenderPid, Message}).

handle_cast({add_client, ClientPid}, State) ->
    Timestamp = erlang:monotonic_time(),
    io:format("Adding client: ~p~n", [ClientPid]),
    ets:insert(?CLIENTS_TABLE, {ClientPid, Timestamp}),
    {noreply, State};

handle_cast({remove_client, ClientPid}, State) ->
    io:format("Removing client: ~p~n", [ClientPid]),
    ets:delete(?CLIENTS_TABLE, ClientPid),
    {noreply, State}.

handle_call({route_message, SenderPid, Message}, _From, State) ->
    ClientPids = ets:tab2list(?CLIENTS_TABLE),
    ClientsExcludingSender = lists:filter(fun({Pid, _}) -> Pid =/= SenderPid end, ClientPids),
    case ClientsExcludingSender of
        [] ->
            SenderPid ! {error, "No clients available to receive the message"},
            {reply, {error, no_clients}, State};
        _ ->
            {ClientPid, _} = lists:nth(rand:uniform(length(ClientsExcludingSender)), ClientsExcludingSender),
            io:format("Routing message to client: ~p~n", [ClientPid]),
            ClientPid ! {message, Message},
            {reply, ok, State}
    end.

handle_info(_Info, State) ->
    {noreply, State}.

terminate(_Reason, _State) ->
    ets:delete(?CLIENTS_TABLE),
    ok.
