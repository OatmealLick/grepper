-module(graph_traversal).

-include("records.hrl").

%% API
-export([traverse/2, spawned_traverse/2, test/0]).

%% @doc
%% Feeds compiled NFA graph with given text and searches for matches.
%% Whenever there is more than one edge emanating from current state labeled with eps new process(es) are created,
%% one for each edge.
%% @end
traverse(Matcher, Text) -> base_traverse(Matcher, Text).

%% @doc
%% base_traverse/2 represents the main line of looking for a match - the one that was spawned first and will
%% try to match beginning with every text character as the first character of the potential match. If a match on base
%% traversal fails, the matcher is reset and traversing continues.
%% @end

% TODO execution should stop here and be handled elsewhere
base_traverse(_Matcher, []) -> {ok, end_of_input};
base_traverse(Matcher, [Char | Text]) ->
  Matchers = traverse_step(Matcher, Char),
  lists:map(fun(M) -> notify_if_match_found(M) end, Matchers),
  case length(Matchers) of
    0 ->
      {ResetMatcher, NextText} = reset(Matcher, [Char | Text]),
      base_traverse(ResetMatcher, NextText);
    _More ->
      [BaseMatcher | OtherMatchers] = Matchers,
      dispatch(OtherMatchers, [Char | Text]),
      base_traverse(BaseMatcher, next_text(BaseMatcher, [Char | Text]))
  end.

%% @doc
%% spawned_traverse/2 represents the diverged line of looking for a match - the ones that were spawned in states with
%% more than 1 emanating edge. If a match on spawned traversal fails, the process dies with dead_end message.
%% @end

% TODO execution should stop here and be handled elsewhere
spawned_traverse(_Matcher, []) -> {ok, end_of_input};
spawned_traverse(Matcher, [Char | Text]) ->
  Matchers = traverse_step(Matcher, Char),
  lists:map(fun(M) -> notify_if_match_found(M) end, Matchers),
  case length(Matchers) of
    0 -> exit(self(), normal);
    _More ->
      [BaseMatcher | OtherMatchers] = Matchers,
      dispatch(OtherMatchers, [Char | Text]),
      spawned_traverse(BaseMatcher, next_text(BaseMatcher, [Char | Text]))
  end.

%% @doc
%% Checks if matcher has reached the matching state and generates the list of matchers that can be created from
%% current state by travelling along one of the emanating paths.
traverse_step(Matcher, Char) ->
  Graph = (Matcher#traversal.graph)#graph.nfa,
  RawEdges = digraph:out_edges(Graph, {vertex, Matcher#traversal.current_state}),
  Edges = lists:map(fun(Edge) -> digraph:edge(Graph, Edge) end, RawEdges),
  PossibleTransitions = lists:filtermap(fun(Edge) -> is_traversable(Edge, Char) end, Edges),
  _Matchers = lists:map(fun(Transition) -> create_next_state_matcher(Matcher, Transition) end, PossibleTransitions).

notify_if_match_found(Matcher) ->
  case match_found(Matcher) of
    true ->
      Match = lists:reverse(Matcher#traversal.matched),
      send_match_report(Match);
    false -> ok
  end.

match_found(Matcher) ->
  CurrentState = Matcher#traversal.current_state,
  ExitState = (Matcher#traversal.graph)#graph.exit,
  CurrentState =:= ExitState.

send_match_report(Match) ->
  io:fwrite("Match: ~p ~n", [Match]),
  {match_found, Match}. % TODO send somewhere

is_traversable({_Edge, _From, {vertex, ToState}, eps}, _Label) -> {true, {eps, ToState}};
is_traversable({_Edge, _From, {vertex, ToState}, Label}, Label) -> {true, {Label, ToState}};
is_traversable(_Edge, _Label) -> false.

create_next_state_matcher(Matcher, {eps, NextState}) ->
  #traversal{graph = Matcher#traversal.graph,
             current_state = NextState,
             matched = Matcher#traversal.matched,
             read = false};
create_next_state_matcher(Matcher, {AcceptedChar, NextState}) ->
  #traversal{graph = Matcher#traversal.graph,
             current_state = NextState,
             matched = [AcceptedChar | Matcher#traversal.matched],
             read = true}.

reset(Matcher, [Char | Text]) ->
  Matched = lists:reverse(Matcher#traversal.matched),
  case length(Matched) of
    0 -> NextText = Text;
    _More ->
      [_SkippedHead | RestoredInput] = Matched,
      NextText = RestoredInput ++ [Char | Text]
  end,
  ResetMatcher = Matcher#traversal{current_state = (Matcher#traversal.graph)#graph.entry,
                                   matched = [],
                                   read = false},
  {ResetMatcher, NextText}.

dispatch(Matchers, Text) ->
  lists:map(fun(Matcher) -> spawn(graph_traversal, spawned_traverse, [Matcher, next_text(Matcher, Text)]) end, Matchers).

next_text(Matcher, [Char | Text]) ->
  case Matcher#traversal.read of
    true -> Text;
    false -> [Char | Text]
  end.


%% TEST

test() ->
  Graph = post2nfa:convert(regex2post:convert("ab*")),
  Matcher = #traversal{graph = Graph, current_state = Graph#graph.entry},
  traverse(Matcher, "abbabcacb").