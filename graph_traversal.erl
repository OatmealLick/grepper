-module(graph_traversal).

-include("records.hrl").

%% API
-export([traverse/2]).

%% @doc
%% Feeds compiled NFA graph with given text and searches for matches.
%% Whenever there is more than one edge emanating from current state labeled with eps new process(es) are created,
%% one for each edge.
%% @end
traverse(Matcher, []) -> {end_of_input, Matcher};
traverse(Matcher, Text) when Matcher#traversal.current_state == (Matcher#traversal.graph)#graph.exit ->
  _MatchReport = {match_found, Matcher#traversal.matched}, % can be sent somewhere
  % don't reset, it will be done if next character cannot be accepted
  traverse(Matcher, Text);
traverse(Matcher, [Char | Text]) ->
  Edges = digraph:out_edges(Matcher#traversal.graph, {vertex, Matcher#traversal.current_state}),
  PossibleTransitions = lists:filtermap(fun(Edge) -> is_traversable(Edge, Char) end, Edges),
  Matchers = lists:map(fun(Transition) -> update_matcher(Matcher, Transition) end, PossibleTransitions),
  case length(Matchers) == 0 of
    true -> reset(Matcher),
            traverse(Matcher, [Char | Text]);
    false -> [BaseMatcher | OtherMatchers] = Matchers,
            dispatch(OtherMatchers, Text),
            traverse(BaseMatcher, Text)
  end.

is_traversable({_Edge, _From, To, eps}, _Label) -> {true, {eps, To}};
is_traversable({_Edge, _From, To, Label}, Label) -> {true, {Label, To}};
is_traversable(_Edge, _Label) -> false.

update_matcher(Matcher, {eps, NextState}) ->
  #traversal{graph = Matcher#traversal.graph,
             current_state = NextState,
             matched = Matcher#traversal.matched};
update_matcher(Matcher, {AcceptedChar, NextState}) ->
  #traversal{graph = Matcher#traversal.graph,
             current_state = NextState,
             matched = [AcceptedChar | Matcher#traversal.matched]}.

reset(Matcher) ->
  Matcher#traversal{current_state = (Matcher#traversal.graph)#graph.entry,
                    matched = []}.

dispatch(Matchers, Text) ->
  lists:map(fun(Matcher) -> spawn(?MODULE, traverse, [Matcher, Text]) end, Matchers).