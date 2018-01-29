-module(grepper_sup).
-behaviour(supervisor).

%% API
-export([start_link/0]).

%% Supervisor callbacks
-export([init/1]).

%%====================================================================
%% API functions
%%====================================================================

start_link() ->
  supervisor:start_link({local, grepper_sup}, ?MODULE, []).

%%====================================================================
%% Supervisor callbacks
%%====================================================================

init([]) ->
  WorkersManager = create_workers_manager(),
  {ok, {{one_for_all, 0, 1}, [WorkersManager]}}.

%%====================================================================
%% Internal functions
%%====================================================================

create_workers_manager() -> #{
  id => workers_manager,
  start => {workers_manager, start_link, [self()]},
  modules => [workers_manager],
  type => worker,
  shutdown => 1500,
  restart => temporary
}.