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
base_traverse(_Matcher, <<>>) -> {ok, end_of_input};
base_traverse(Matcher, Text = <<Char, _Rest/bitstring>>) ->
  Matchers = traverse_step(Matcher, Char),
  lists:map(fun(M) -> notify_if_match_found(M) end, Matchers),
  case length(Matchers) of
    0 ->
      {ResetMatcher, NextText} = reset(Matcher, Text),
      base_traverse(ResetMatcher, NextText);
    _More ->
      [BaseMatcher | OtherMatchers] = Matchers,
      dispatch(OtherMatchers, Text),
      base_traverse(BaseMatcher, next_text(BaseMatcher, Text))
  end.

%% @doc
%% spawned_traverse/2 represents the diverged line of looking for a match - the ones that were spawned in states with
%% more than 1 emanating edge. If a match on spawned traversal fails, the process dies with dead_end message.
%% @end

% TODO execution should stop here and be handled elsewhere
spawned_traverse(_Matcher, <<>>) -> {ok, end_of_input};
spawned_traverse(Matcher, Text = <<Char, _Rest/bitstring>>) ->
  Matchers = traverse_step(Matcher, Char),
  lists:map(fun(M) -> notify_if_match_found(M) end, Matchers),
  case length(Matchers) of
    0 -> exit(self(), normal);
    _More ->
      [BaseMatcher | OtherMatchers] = Matchers,
      dispatch(OtherMatchers, Text),
      spawned_traverse(BaseMatcher, next_text(BaseMatcher, Text))
  end.

%% @doc
%% Checks if matcher has reached the matching state and generates the list of matchers that can be created from
%% current state by travelling along one of the emanating paths.
traverse_step(M, Char) ->
  Graph = (M#matcher.graph)#graph.nfa,
  RawEdges = digraph:out_edges(Graph, {vertex, M#matcher.current_state}),
  Edges = lists:map(fun(Edge) -> digraph:edge(Graph, Edge) end, RawEdges),
  PossibleTransitions = lists:filtermap(fun(Edge) -> is_traversable(Edge, Char) end, Edges),
  _Matchers = lists:map(fun(Transition) -> create_next_state_matcher(M, Transition) end, PossibleTransitions).

notify_if_match_found(M) ->
  case match_found(M) of
    true ->
      Match = lists:reverse(M#matcher.matched),
      send_match_report(Match);
    false -> ok
  end.

match_found(M) ->
  CurrentState = M#matcher.current_state,
  ExitState = (M#matcher.graph)#graph.exit,
  CurrentState =:= ExitState.

send_match_report(Match) ->
  io:fwrite("Match: ~p ~n", [Match]),
  {match_found, Match}. % TODO send somewhere

is_traversable({_Edge, _From, {vertex, ToState}, eps}, _Label) -> {true, {eps, ToState}};
is_traversable({_Edge, _From, {vertex, ToState}, Label}, Label) -> {true, {Label, ToState}};
is_traversable(_Edge, _Label) -> false.

create_next_state_matcher(M, {eps, NextState}) ->
  #matcher{graph         = M#matcher.graph,
           current_state = NextState,
           matched       = M#matcher.matched,
           read          = false};
create_next_state_matcher(M, {AcceptedChar, NextState}) ->
  #matcher{graph         = M#matcher.graph,
           current_state = NextState,
           matched       = [AcceptedChar | M#matcher.matched],
           read          = true}.

reset(M, Text = <<_Char, Rest/bitstring>>) ->
  Matched = lists:reverse(M#matcher.matched),
  case length(Matched) of
    0 -> NextText = Rest;
    _More ->
      <<_H,RestoredInput/bitstring>> = list_to_binary(Matched),
      NextText = <<RestoredInput/bitstring, Text/bitstring>>
  end,
  ResetMatcher = M#matcher{current_state = (M#matcher.graph)#graph.entry,
                           matched       = [],
                           read          = false},
  {ResetMatcher, NextText}.

dispatch(Matchers, Text) ->
  lists:map(fun(Matcher) -> spawn(graph_traversal, spawned_traverse, [Matcher, next_text(Matcher, Text)]) end, Matchers).

next_text(M, Text = <<_H, Rest/bitstring>>) ->
  case M#matcher.read of
    true -> Rest;
    false -> Text
  end.


%% TEST

test() ->
  Graph = post2nfa:convert(regex2post:convert("ab*")),
  Matcher = #matcher{graph = Graph, current_state = Graph#graph.entry},
  traverse(Matcher, <<"abbabcacb">>).