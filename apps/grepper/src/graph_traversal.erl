-module(graph_traversal).
-include("records.hrl").

%% API
-export([traverse/2, temp_traverse/2]).


%% @doc
%% traverse/2 represents the main line of looking for a match - the one that was spawned first in a given file
%% part and will try to find match starting with each text character as the first character of the potential match.
%% If a match on traverse/2 fails, the matcher is reset and traversing continues.
traverse(Matcher, <<>>) -> {ok, end_of_input, Matcher};
traverse(Matcher, Text = <<Char, _Rest/bitstring>>) ->
  searcher:notify_if_match_found(Matcher),
  Matchers = traverse_step(Matcher, Char),
  case length(Matchers) of
    0 ->
      {ResetMatcher, NextText} = reset(Matcher, Text),
      traverse(ResetMatcher, NextText);
    _More ->
      diverge(tl(Matchers), Text),
      traverse(hd(Matchers), next_text(hd(Matchers), Text))
  end.


%% @doc
%% temp_traverse/2 represents the diverged lines of looking for a match - the ones that were spawned in states with
%% more than 1 emanating edge. If a match on spawned traversal fails, the process job is finished.
temp_traverse(Matcher, <<>>) -> {ok, end_of_input, Matcher};
temp_traverse(Matcher, Text = <<Char, _Rest/bitstring>>) ->
  searcher:notify_if_match_found(Matcher),
  Matchers = traverse_step(Matcher, Char),
  case length(Matchers) of
    0 -> {ok, finished, Matcher};
    _More ->
      diverge(tl(Matchers), Text),
      temp_traverse(hd(Matchers), next_text(hd(Matchers), Text))
  end.


%% @doc
%% Generates the list of matchers that can be created from current state by travelling along one of the emanating paths.
traverse_step(M, Char) ->
  Graph = (M#matcher.graph)#graph.nfa,
  RawEdges = digraph:out_edges(Graph, {vertex, M#matcher.current_state}),
  Edges = lists:map(fun(Edge) -> digraph:edge(Graph, Edge) end, RawEdges),
  PossibleTransitions = lists:filtermap(fun(Edge) -> is_traversable(Edge, Char) end, Edges),
  _Matchers = lists:map(fun(Transition) -> create_next_state_matcher(M, Transition) end, PossibleTransitions).


%% @doc
%% Checks if given edge can be traversed on given character (Label).
is_traversable({_Edge, _From, {vertex, ToState}, eps}, _Label) -> {true, {eps, ToState}};
is_traversable({_Edge, _From, {vertex, ToState}, Label}, Label) -> {true, {Label, ToState}};
is_traversable(_Edge, _Label) -> false.


%% @doc
%% Creates next state of matcher after it traverses along the given edge.
create_next_state_matcher(M, {eps, NextState}) ->
  #matcher{graph = M#matcher.graph,
           current_state = NextState,
           matched = M#matcher.matched,
           read = false,
           part_id = M#matcher.part_id};
create_next_state_matcher(M, {AcceptedChar, NextState}) ->
  #matcher{graph = M#matcher.graph,
           current_state = NextState,
           matched = [AcceptedChar | M#matcher.matched],
           read = true,
           part_id = M#matcher.part_id}.


%% @doc
%% Resets matcher when matching failed so it will resume searching from the next character to the one from which the
%% failed match started.
reset(M, Text = <<_Char, Rest/bitstring>>) ->
  Matched = lists:reverse(M#matcher.matched),
  case length(Matched) of
    0 -> NextText = Rest;
    _More ->
      <<_H, RestoredInput/bitstring>> = list_to_binary(Matched),
      NextText = <<RestoredInput/bitstring, Text/bitstring>>
  end,
  ResetMatcher = M#matcher{current_state = (M#matcher.graph)#graph.entry,
                           matched = [],
                           read = false},
  {ResetMatcher, NextText}.


%% @doc
%% Spawns searching processes on divergence points of the graph.
diverge(Matchers, Text) ->
  lists:foreach(fun(Matcher) -> workers_manager:spawn_worker(Matcher, next_text(Matcher, Text)) end, Matchers).


%% @doc
%% Drops first character from the analyzed text if the matcher recently read it.
next_text(M, Text = <<_H, Rest/bitstring>>) ->
  case M#matcher.read of
    true -> Rest;
    false -> Text
  end.