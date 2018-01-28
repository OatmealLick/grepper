-module(workers_manager).
-behaviour(gen_server).
-include("records.hrl").

%% API
-export([start_link/1,
         report_match/1,
         get_next_part/1,
         spawn_worker/2]).

%% gen_server callbacks
-export([init/1,
         handle_call/3,
         handle_cast/2,
         handle_info/2,
         terminate/2,
         code_change/3]).

-record(state, {workers_sup,
                workers_refs = [],
                total_workers_count = 0,
                resources = #{},
                found = 0,
                start_timestamp,
                end_timestamp}).

%%%===================================================================
%%% API
%%%===================================================================

start_link(SupervisorPid) ->
  gen_server:start_link({local, ?MODULE}, ?MODULE, {SupervisorPid}, []).

report_match(Match) ->
  gen_server:cast(?MODULE, {match, Match}).

get_next_part(M) ->
  gen_server:call(?MODULE, {next_part, M#matcher.part_id}).

spawn_worker(Matcher, Text) ->
  gen_server:cast(?MODULE, {spawn_worker, Matcher, Text}).

%%%===================================================================
%%% gen_server callbacks
%%%===================================================================

init({SupervisorPid}) ->
  self() ! {start_workers_supervisor, SupervisorPid},
  self() ! {start_workers},
  {ok, #state{start_timestamp = os:timestamp()}}.

handle_call({next_part, ID}, _From, S) ->
  Reply = maps:get(ID+1, S#state.resources, last_part),
  {reply, Reply, S};

handle_call(_Request, _From, State) ->
  {reply, ok, State}.

handle_cast({match, Match}, S) ->
  io:fwrite("Found: ~p~n", [Match]),
  {noreply, S#state{found = S#state.found + 1}};

handle_cast({spawn_worker, Matcher, Text}, State) ->
  NewState = start_spawned_worker(Matcher, Text, State),
  {noreply, NewState};

handle_cast(_Request, State) ->
  {noreply, State}.

handle_info({start_workers_supervisor, SupPid}, S) ->
  WorkersSupervisor = create_workers_supervisor(),
  {ok, Pid} = supervisor:start_child(SupPid, WorkersSupervisor),
  {noreply, S#state{workers_sup = Pid}};

handle_info({start_workers}, State) ->
  {ok, Filepath} = application:get_env(file),
  {ok, Parts} = application:get_env(parts),
  FileParts = file_slicer:slice(Filepath, Parts),
  IDFileParts = lists:zipwith(fun (Id, Part) -> {Id, Part} end, lists:seq(1, length(FileParts)), FileParts),
  NewState = lists:foldl(fun start_worker/2, State, IDFileParts),
  {noreply, NewState};

handle_info({'DOWN', Ref, process, _Pid, _Reason}, S) ->
  NextState = S#state{workers_refs = lists:delete(Ref, S#state.workers_refs)},
  case NextState#state.workers_refs of
    [] ->
      print_summary(NextState#state{end_timestamp = os:timestamp()}),
      cleanup(NextState);
    _Else -> {noreply, NextState}
  end.

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

start_worker({ID, FilePart}, S = #state{workers_sup = Sup}) ->
  Matcher = build_matcher(ID),
  {ok, Pid} = supervisor:start_child(Sup, [base, Matcher, FilePart]),
  Ref = erlang:monitor(process, Pid),
  S#state{workers_refs = [Ref | S#state.workers_refs],
          total_workers_count = S#state.total_workers_count + 1,
          resources = maps:put(ID, FilePart, S#state.resources)}.

start_spawned_worker(Matcher, Text, S = #state{workers_sup = Sup}) ->
  {ok, Pid} = supervisor:start_child(Sup, [spawned, Matcher, Text]),
  Ref = erlang:monitor(process, Pid),
  S#state{workers_refs = [Ref | S#state.workers_refs],
          total_workers_count = S#state.total_workers_count + 1}.

build_matcher(PartID) ->
  {ok, Regex} = application:get_env(regex),
  G = post2nfa:convert(regex2post:convert(Regex)),
  #matcher{part_id = PartID,
           graph = G,
           current_state = G#graph.entry}.

print_summary(S) ->
  io:fwrite(" Number of spawned workers: ~p~n", [S#state.total_workers_count]),
  io:fwrite("Number of occurances found: ~p~n", [S#state.found]),
  io:fwrite("Search time (min:sec.mics): ~p:~p.~p~n", format_elapsed_time(S)).

format_elapsed_time(S) ->
  Mics = timer:now_diff(S#state.end_timestamp, S#state.start_timestamp),
  Secs = Mics div 1000000,
  NewMics = Mics - Secs*1000000,
  Mins = Secs div 60,
  NewSecs = Secs - Mins*60,
  [Mins, NewSecs, NewMics].

cleanup(S) ->
  exit(S#state.workers_sup, shutdown),
  application:stop(grepper).
