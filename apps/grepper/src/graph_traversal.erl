-module(graph_traversal).
-include("records.hrl").

%% API
-export([traverse/2, temp_traverse/2, test/0]).

%% @doc
%% Feeds compiled NFA graph with given text and searches for matches.
%% Whenever there is more than one edge emanating from current state labeled with eps new process(es) are created,
%% one for each edge.
%% @end

%% @doc
%% traverse/2 represents the main line of looking for a match - the one that was spawned first and will
%% try to match beginning with every text character as the first character of the potential match. If a match on base
%% traversal fails, the matcher is reset and traversing continues.
%% @end

traverse(Matcher, <<>>) -> {ok, end_of_input, Matcher};
traverse(Matcher, Text = <<Char, _Rest/bitstring>>) ->
  searcher:notify_if_match_found(Matcher),
  Matchers = traverse_step(Matcher, Char),
  case length(Matchers) of
    0 ->
      {ResetMatcher, NextText} = reset(Matcher, Text),
      traverse(ResetMatcher, NextText);
    _More ->
      dispatch(tl(Matchers), Text),
      traverse(hd(Matchers), next_text(hd(Matchers), Text))
  end.

%% @doc
%% temp_traverse/2 represents the diverged line of looking for a match - the ones that were spawned in states with
%% more than 1 emanating edge. If a match on spawned traversal fails, the process dies with dead_end message.
%% @end

temp_traverse(Matcher, <<>>) -> {ok, end_of_input, Matcher};
temp_traverse(Matcher, Text = <<Char, _Rest/bitstring>>) ->
  searcher:notify_if_match_found(Matcher),
  Matchers = traverse_step(Matcher, Char),
  case length(Matchers) of
    0 -> {ok, finished, Matcher};
    _More ->
      dispatch(tl(Matchers), Text),
      temp_traverse(hd(Matchers), next_text(hd(Matchers), Text))
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

is_traversable({_Edge, _From, {vertex, ToState}, eps}, _Label) -> {true, {eps, ToState}};
is_traversable({_Edge, _From, {vertex, ToState}, Label}, Label) -> {true, {Label, ToState}};
is_traversable(_Edge, _Label) -> false.

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

dispatch(Matchers, Text) ->
  lists:foreach(fun(Matcher) -> workers_manager:spawn_worker(Matcher, next_text(Matcher, Text)) end, Matchers).

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