-module(workers_manager).

-behaviour(gen_server).

%% API
-export([start_link/1]).

%% gen_server callbacks
-export([init/1,
         handle_call/3,
         handle_cast/2,
         handle_info/2,
         terminate/2,
         code_change/3]).

-record(state, {workers_sup,
                workers_refs = [],
                found = 0,
                searching = 0}).

%%%===================================================================
%%% API
%%%===================================================================

start_link(SupervisorPid) ->
  gen_server:start_link({local, ?MODULE}, ?MODULE, {SupervisorPid}, []).

%%%===================================================================
%%% gen_server callbacks
%%%===================================================================

init({SupervisorPid}) ->
  self() ! {start_workers_supervisor, SupervisorPid},
  self() ! {start_workers},
  {ok, #state{}}.

handle_call(_Request, _From, State) ->
  {reply, ok, State}.

handle_cast({match, Match}, State) ->
  io:fwrite("Found: ~p~n", [Match]),
  {noreply, State#state{found = State#state.found + 1}};

handle_cast({finished}, State) ->
  RemainingParts = State#state.searching - 1,
  io:fwrite("* Part finished, remaining: ~p~n", [RemainingParts]),
  case RemainingParts of
    0 -> io:fwrite("# Finished, total matches found: ~p~n", [State#state.found]);
    _Else -> ok
  end,
  {noreply, State#state{searching = RemainingParts}};

handle_cast(_Request, State) ->
  {noreply, State}.

handle_info({start_workers_supervisor, SupPid}, S) ->
  WorkersSupervisor = create_workers_supervisor(),
  {ok, Pid} = supervisor:start_child(SupPid, WorkersSupervisor),
  link(Pid),
  {noreply, S#state{workers_sup = Pid}};

handle_info({start_workers}, State) ->
  {ok, Filepath} = application:get_env(file),
  {ok, Parts} = application:get_env(parts),
  FileParts = file_slicer:slice(Filepath, Parts),
  NewState = lists:foldl(fun start_worker/2, State, FileParts),
  {noreply, NewState};

handle_info(_Info, State) ->
  {noreply, State}.

terminate(_Reason, _State) ->
  ok.

code_change(_OldVsn, State, _Extra) ->
  {ok, State}.

%%%===================================================================
%%% Internal functions
%%%===================================================================

create_workers_supervisor() -> #{
  id => workers_supervisor,
  start => {workers_supervisor, start_link, []},
  modules => [workers_supervisor],
  type => supervisor,
  shutdown => 1500,
  restart => temporary
}.

start_worker(_FilePart, S = #state{workers_sup = Sup}) ->
  {ok, Pid} = supervisor:start_child(Sup, []),
  Ref = erlang:monitor(process, Pid),
  S#state{workers_refs = [Ref| S#state.workers_refs]}.