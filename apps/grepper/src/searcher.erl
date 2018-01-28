-module(searcher).
-behaviour(gen_server).
-include("records.hrl").

%% API
-export([start_link/3,
         notify_if_match_found/1]).

%% gen_server callbacks
-export([init/1,
         handle_call/3,
         handle_cast/2,
         handle_info/2,
         terminate/2,
         code_change/3]).

-record(state, {}).

%%%===================================================================
%%% API
%%%===================================================================

start_link(base, PartID, FilePart) ->
  gen_server:start_link(?MODULE, {base, PartID, FilePart}, []);

start_link(spawned, Matcher, Text) ->
  gen_server:start_link(?MODULE, {spawned, Matcher, Text}, []).

%%%===================================================================
%%% gen_server callbacks
%%%===================================================================

init({base, PartID, FilePart}) ->
  self() ! {run, PartID, FilePart},
  {ok, #state{}};

init({spawned, Matcher, Text}) ->
  self() ! {temp_run, Matcher, Text},
  {ok, #state{}}.

handle_call(_Request, _From, State) ->
  {reply, ok, State}.

handle_cast(_Request, State) ->
  {noreply, State}.

handle_info({run, PartID, FilePart}, State) ->
  Matcher = build_matcher(PartID),
  do_traverse(traverse, [Matcher, FilePart]),
  {noreply, State};

handle_info({temp_run, Matcher, Text}, State) ->
  do_traverse(temp_traverse, [Matcher, Text]),
  {noreply, State};

handle_info({run_continuation, #matcher{matched = []}}, State) ->
  {noreply, State};
handle_info({run_continuation, M}, State) ->
  case workers_manager:get_next_part(M) of
    last_part ->
      notify_if_match_found(M),
      exit(normal);
    NextPart -> do_traverse(temp_traverse, [M, NextPart])
  end,
  {noreply, State};

handle_info(_Info, State) ->
  {noreply, State}.

terminate(_Reason, _State) ->
  ok.

code_change(_OldVsn, State, _Extra) ->
  {ok, State}.

%%%===================================================================
%%% Internal functions
%%%===================================================================

build_matcher(PartID) ->
  {ok, Regex} = application:get_env(regex),
  G = post2nfa:convert(regex2post:convert(Regex)),
  #matcher{part_id = PartID,
           graph = G,
           current_state = G#graph.entry}.

do_traverse(TraverseFuncName, Args) ->
  case apply(graph_traversal, TraverseFuncName, Args) of
    {ok, end_of_input, EndMatcher} ->
      self() ! {run_continuation, EndMatcher};
    {ok, finished, EndMatcher} -> exit(normal)
  end.

notify_if_match_found(M) ->
  case match_found(M) of
    true ->
      Match = lists:reverse(M#matcher.matched),
      workers_manager:report_match(Match);
    false -> ok
  end.

match_found(M) ->
  CurrentState = M#matcher.current_state,
  ExitState = (M#matcher.graph)#graph.exit,
  CurrentState =:= ExitState.