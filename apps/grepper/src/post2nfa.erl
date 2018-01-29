-module(post2nfa).
-include_lib("eunit/include/eunit.hrl").
-include("records.hrl").

%% API
-export([convert/1]).


convert(Input) -> convert(Input, [], 0).
convert([], [BuiltGraph | _], _) -> BuiltGraph;
convert([Char | Input], Stack, CurrentState) ->
  case Char of
    $. -> {StackNew, CurrentStateNew} = process_concatenation(Stack, CurrentState),
      convert(Input, StackNew, CurrentStateNew);
    $+ -> {StackNew, CurrentStateNew} = process_alternation(Stack, CurrentState),
      convert(Input, StackNew, CurrentStateNew);
    $* -> {StackNew, CurrentStateNew} = process_multiplication(Stack, CurrentState),
      convert(Input, StackNew, CurrentStateNew);
    CharLiteral -> {StackNew, CurrentStateNew} = process_literal(CharLiteral, Stack, CurrentState),
      convert(Input, StackNew, CurrentStateNew)
  end.


merge(Graph1, Graph2) ->
  GraphWithVertices = add_vertices(Graph1, digraph:vertices(Graph2)),
  add_edges(GraphWithVertices, Graph2, digraph:edges(Graph2)).


add_vertices(Graph, []) -> Graph;
add_vertices(Graph, [Vertex | Tail]) ->
  digraph:add_vertex(Graph, Vertex),
  add_vertices(Graph, Tail).


add_edges(Graph, _AddedGraph, []) -> Graph;
add_edges(Graph, AddedGraph, [Edge | Tail]) ->
  {_, Vertex1, Vertex2, Label} = digraph:edge(AddedGraph, Edge), % use 2 second graph to obtain edge
  digraph:add_edge(Graph, Vertex1, Vertex2, Label),
  add_edges(Graph, AddedGraph, Tail).


process_concatenation([Second, First | Tail], State) ->
  Graph = merge(First#graph.nfa, Second#graph.nfa),
  digraph:add_edge(Graph, {vertex, First#graph.exit}, {vertex, Second#graph.entry}, eps),
  ConcatenationGraph = #graph{nfa   = Graph,
                              entry = First#graph.entry,
                              exit  = Second#graph.exit},
  {[ConcatenationGraph | Tail], State}.


process_alternation([Second, First | Tail], State) ->
  Graph = merge(First#graph.nfa, Second#graph.nfa),
  EntryState = State + 1,
  ExitState = State + 2,
  StartVertex = digraph:add_vertex(Graph, {vertex, EntryState}),
  EndVertex = digraph:add_vertex(Graph, {vertex, ExitState}),
  digraph:add_edge(Graph, StartVertex, {vertex, Second#graph.entry}, eps),
  digraph:add_edge(Graph, StartVertex, {vertex, First#graph.entry}, eps),
  digraph:add_edge(Graph, {vertex, Second#graph.exit}, EndVertex, eps),
  digraph:add_edge(Graph, {vertex, First#graph.exit}, EndVertex, eps),
  AlternationGraph = #graph{nfa   = Graph,
                            entry = EntryState,
                            exit  = ExitState},
  {[AlternationGraph | Tail], ExitState}.


process_multiplication([Top | Tail], State) ->
  LoopState = State + 1,
  NewVertex = digraph:add_vertex(Top#graph.nfa, {vertex, LoopState}),
  digraph:add_edge(Top#graph.nfa, NewVertex, {vertex, Top#graph.entry}, eps),
  digraph:add_edge(Top#graph.nfa, {vertex, Top#graph.exit}, NewVertex, eps),
  MultiplicationGraph = #graph{nfa   = Top#graph.nfa,
                               entry = LoopState,
                               exit  = LoopState},
  {[MultiplicationGraph | Tail], LoopState}.


process_literal(CharLiteral, Stack, State) ->
  Graph = digraph:new(),
  EntryState = State + 1,
  ExitState = State + 2,
  StartVertex = digraph:add_vertex(Graph, {vertex, EntryState}),
  EndVertex = digraph:add_vertex(Graph, {vertex, ExitState}),
  digraph:add_edge(Graph, StartVertex, EndVertex, CharLiteral),
  CharLiteralGraph = #graph{nfa   = Graph,
                            entry = EntryState,
                            exit  = ExitState},
  {[CharLiteralGraph | Stack], ExitState}.


%% TEST

equal_graph_records(ExpectedGraphRecord, ActualGraphRecord) ->
  SameGraphs = equal_graphs(ExpectedGraphRecord#graph.nfa, ActualGraphRecord#graph.nfa),
  SameEntryStates = ExpectedGraphRecord#graph.entry =:= ActualGraphRecord#graph.entry,
  SameExitStates = ExpectedGraphRecord#graph.exit =:= ActualGraphRecord#graph.exit,
  SameGraphs and SameEntryStates and SameExitStates.


equal_graphs(ExpectedGraph, ActualGraph) ->
  SameVertices = digraph:vertices(ExpectedGraph) =:= digraph:vertices(ActualGraph),
  SameEdges = digraph:vertices(ExpectedGraph) =:= digraph:vertices(ActualGraph),
  SameVertices and SameEdges.


literal_test() ->
  Graph = digraph:new(),
  V1 = {vertex, 1},
  V2 = {vertex, 2},
  Label = $a,
  digraph:add_vertex(Graph, V1),
  digraph:add_vertex(Graph, V2),
  digraph:add_edge(Graph, V1, V2, Label),
  ExpectedGraph = #graph{nfa = Graph, entry = 1, exit = 2},
  TestedGraph = post2nfa:convert("a"),
  ?assert(equal_graph_records(ExpectedGraph, TestedGraph)).


multiplication_test() ->
  Graph = digraph:new(),
  V1 = {vertex, 1},
  V2 = {vertex, 2},
  Label = $a,
  LoopV = {vertex, 3},
  digraph:add_vertex(Graph, V1),
  digraph:add_vertex(Graph, V2),
  digraph:add_vertex(Graph, LoopV),
  digraph:add_edge(Graph, V1, V2, Label),
  digraph:add_edge(Graph, LoopV, V1, eps),
  digraph:add_edge(Graph, V2, LoopV, eps),
  ExpectedGraph = #graph{nfa = Graph, entry = 3, exit = 3},
  TestedGraph = post2nfa:convert("a*"),
  ?assert(equal_graph_records(ExpectedGraph, TestedGraph)).


concatenation_test() ->
  Graph = digraph:new(),
  V1 = {vertex, 1},
  V2 = {vertex, 2},
  V3 = {vertex, 3},
  V4 = {vertex, 4},
  L1 = $a,
  L2 = $b,
  digraph:add_vertex(Graph, V1),
  digraph:add_vertex(Graph, V2),
  digraph:add_vertex(Graph, V3),
  digraph:add_vertex(Graph, V4),
  digraph:add_edge(Graph, V1, V2, L1),
  digraph:add_edge(Graph, V2, V3, eps),
  digraph:add_edge(Graph, V3, V4, L2),
  ExpectedGraph = #graph{nfa = Graph, entry = 1, exit = 4},
  TestedGraph = post2nfa:convert("ab."),
  ?assert(equal_graph_records(ExpectedGraph, TestedGraph)).


alternation_test() ->
  Graph = digraph:new(),
  V1 = {vertex, 1},
  V2 = {vertex, 2},
  V3 = {vertex, 3},
  V4 = {vertex, 4},
  EntryV = {vertex, 5},
  ExitV = {vertex, 6},
  L1 = $a,
  L2 = $b,
  digraph:add_vertex(Graph, V1),
  digraph:add_vertex(Graph, V2),
  digraph:add_vertex(Graph, V3),
  digraph:add_vertex(Graph, V4),
  digraph:add_vertex(Graph, EntryV),
  digraph:add_vertex(Graph, ExitV),
  digraph:add_edge(Graph, V1, V2, L1),
  digraph:add_edge(Graph, V3, V4, L2),
  digraph:add_edge(Graph, EntryV, V3, eps),
  digraph:add_edge(Graph, EntryV, V1, eps),
  digraph:add_edge(Graph, V4, ExitV, eps),
  digraph:add_edge(Graph, V2, ExitV, eps),
  ExpectedGraph = #graph{nfa = Graph, entry = 5, exit = 6},
  TestedGraph = post2nfa:convert("ab+"),
  ?assert(equal_graph_records(ExpectedGraph, TestedGraph)).