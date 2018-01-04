-module(graph_traversal).

-include("records.hrl").

%% API
-export([traverse/2]).

traverse(Matcher, Text) -> base_traverse(Matcher, Text).

%% @doc
%% Feeds compiled NFA graph with given text and searches for matches.
%% Whenever there is more than one edge emanating from current state labeled with eps new process(es) are created,
%% one for each edge.
%% @end
traverse_step(Matcher, []) -> throw({end_of_input, Matcher}); % TODO execution should stop here and be handled elsewhere
traverse_step(Matcher, [Char | _Rest]) ->
  case match_found(Matcher) of
    true -> Match = lists:reverse(Matcher#traversal.matched),
            send_match_report(Match);
    false -> ok
  end,
  Edges = digraph:out_edges(Matcher#traversal.graph, {vertex, Matcher#traversal.current_state}),
  PossibleTransitions = lists:filtermap(fun(Edge) -> is_traversable(Edge, Char) end, Edges),
  _Matchers = lists:map(fun(Transition) -> create_next_state_matcher(Matcher, Transition) end, PossibleTransitions).

%% @doc
%% base_traverse/2 represents the main line of looking for a match - the one that was spawned first and will
%% try to match beginning with every text character as the first character of the potential match. If a match on base
%% traversal fails, the matcher is reset and traversing continues.
%% @end
base_traverse(Matcher, Text) ->
  Matchers = traverse_step(Matcher, Text),
  case length(Matchers) == 0 of
    true ->
      {UpdatedMatcher, RestoredInput} = reset(Matcher),
      base_traverse(UpdatedMatcher, RestoredInput ++ Text);
    false ->
      [BaseMatcher | OtherMatchers] = Matchers,
      dispatch(OtherMatchers, Text),
      base_traverse(BaseMatcher, Text)
  end.

%% @doc
%% spawned_traverse/2 represents the diverged line of looking for a match - the ones that were spawned in states with
%% more than 1 emanating edge. If a match on spawned traversal fails, the process dies with dead_end message.
%% @end
spawned_traverse(Matcher, Text) ->
  Matchers = traverse_step(Matcher, Text),
  case length(Matchers) == 0 of
    true -> dead_end;
    false ->
      [BaseMatcher | OtherMatchers] = Matchers,
      dispatch(OtherMatchers, Text),
      spawned_traverse(BaseMatcher, Text)
  end.

match_found(Matcher) -> Matcher#traversal.current_state == (Matcher#traversal.graph)#graph.exit.

send_match_report(Match) ->
  {match_found, Match}. % TODO send somewhere

is_traversable({_Edge, _From, To, eps}, _Label) -> {true, {eps, To}};
is_traversable({_Edge, _From, To, Label}, Label) -> {true, {Label, To}};
is_traversable(_Edge, _Label) -> false.

create_next_state_matcher(Matcher, {eps, NextState}) ->
  #traversal{graph = Matcher#traversal.graph,
             current_state = NextState,
             matched = Matcher#traversal.matched};
create_next_state_matcher(Matcher, {AcceptedChar, NextState}) ->
  #traversal{graph = Matcher#traversal.graph,
             current_state = NextState,
             matched = [AcceptedChar | Matcher#traversal.matched]}.

reset(Matcher) ->
  [_SkippedHead | RestoredInput] = lists:reverse(Matcher#traversal.matched),
  UpdatedMatcher = Matcher#traversal{current_state = (Matcher#traversal.graph)#graph.entry,
                                     matched = []},
  {UpdatedMatcher, RestoredInput}.

dispatch(Matchers, Text) ->
  lists:map(fun(Matcher) -> spawn(graph_traversal, spawned_traverse, [Matcher, Text]) end, Matchers).