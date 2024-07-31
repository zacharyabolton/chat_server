%%%-------------------------------------------------------------------
%% @doc config module
%% @end
%%%-------------------------------------------------------------------

-module(config).

-export([get_env/2]).

%% Fetches environment variable value with a default fallback
get_env(Var, Default) ->
  case os:getenv(Var) of
    false -> Default;
    Value -> Value
  end.
