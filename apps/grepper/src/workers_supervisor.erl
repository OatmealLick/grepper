-module(workers_supervisor).
-behaviour(supervisor).

%% API
-export([start_link/0]).

%% Supervisor callbacks
-export([init/1]).

%%%===================================================================
%%% API functions
%%%===================================================================

start_link() ->
  supervisor:start_link({local, ?MODULE}, ?MODULE, []).

%%%===================================================================
%%% Supervisor callbacks
%%%===================================================================

init([]) ->
  {ok, {{simple_one_for_one, 0, 1}, [create_worker_template()]}}.

%%%===================================================================
%%% Internal functions
%%%===================================================================

%% FIXME id
create_worker_template() -> #{
  id => searcher,
  start => {searcher, start_link, []},
  modules => [searcher],
  type => worker,
  shutdown => 2000,
  restart => temporary
}.