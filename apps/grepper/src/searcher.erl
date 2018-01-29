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

start_link(base, Matcher, Text) ->
  gen_server:start_link(?MODULE, {base, Matcher, Text}, []);

start_link(spawned, Matcher, Text) ->
  gen_server:start_link(?MODULE, {spawned, Matcher, Text}, []).

%%%===================================================================
%%% gen_server callbacks
%%%===================================================================

%% @doc
%% Creates base searching process that will continue searching in entire given text.
init({base, Matcher, Text}) ->
  self() ! {run, Matcher, Text},
  {ok, #state{}};

%% @doc
%% Creates spawned searching process that will continue searching until matching fails.
init({spawned, Matcher, Text}) ->
  self() ! {temp_run, Matcher, Text},
  {ok, #state{}}.


handle_call(_Request, _From, State) ->
  {reply, ok, State}.


handle_cast(_Request, State) ->
  {noreply, State}.


handle_info({run, Matcher, Text}, State) ->
  do_traverse(traverse, [Matcher, Text], State);

handle_info({temp_run, Matcher, Text}, State) ->
  do_traverse(temp_traverse, [Matcher, Text], State);

%% @doc
%% Called when input for a given process ends. If matcher is in the process of matching it will be provided with the
%% next chunk of input (from the next part to which file has been split) and traverse it until it fails.
handle_info({run_continuation, #matcher{matched = []}}, State) ->
  {stop, normal, State};
handle_info({run_continuation, M}, State) ->
  case workers_manager:get_next_part(M) of
    last_part ->
      notify_if_match_found(M),
      {stop, normal, State};
    NextPart -> do_traverse(temp_traverse, [M, NextPart], State)
  end;

handle_info(_Info, State) ->
  {noreply, State}.


terminate(_Reason, _State) ->
  ok.


code_change(_OldVsn, State, _Extra) ->
  {ok, State}.

%%%===================================================================
%%% Internal functions
%%%===================================================================

%% @doc
%% Performs traverse (@see graph_traversal:traverse/2) or temporal traverse (@see graph_traversal:temp_traverse/2)
%% and handles its outcome continuing search or ending the process.
do_traverse(TraverseFuncName, Args, State) ->
  case apply(graph_traversal, TraverseFuncName, Args) of
    {ok, end_of_input, EndMatcher} ->
      self() ! {run_continuation, EndMatcher},
      {noreply, State};
    {ok, finished, _} ->
      {stop, normal, State}
  end.


%% @doc
%% Notifies workers manager if given Matcher found the match.
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