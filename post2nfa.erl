-module(post2nfa).
-include_lib("eunit/include/eunit.hrl").
-include("records.hrl").

%% API
-export([convert/1, merge/2, geta/0, getb/0, add_vertices/2, getg/0]).


getg() -> merge(geta(), getb()).

geta() ->
  Graph = digraph:new(),
  V4 = digraph:add_vertex(Graph, {vertex, 4}),
  V5 = digraph:add_vertex(Graph, {vertex, 5}),
  V6 = digraph:add_vertex(Graph, {vertex, 6}),
  digraph:add_edge(Graph, V5, V6, "a"),
  digraph:add_edge(Graph, V6, V4, "c"),
  digraph:add_edge(Graph, V4, V5, "b"),
  Graph.


getb() ->
  Graph = digraph:new(),
  V2 = digraph:add_vertex(Graph, {vertex, 2}),
  V3 = digraph:add_vertex(Graph, {vertex, 3}),
  digraph:add_edge(Graph, V2, V3, "f"),
  digraph:add_edge(Graph, V3, V2, "g"),
  Graph.


convert(Input) -> convert(Input, [], 0).
convert([], [{Head, _, _} | _], _) -> Head;
convert([Char | Input], Stack, CurrentState) ->
  io:fwrite("Char: "),
  io:write(Char),
  io:fwrite("   "),
  io:fwrite("Stack: "),
  io:write(Stack),
  io:fwrite("   "),
  io:fwrite("State: "),
  io:write(CurrentState),
  io:fwrite("\n"),
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
  Graph1 = add_vertices(Graph1, digraph:vertices(Graph2)),
  add_edges(Graph1, digraph:edges(Graph2)).


add_vertices(Graph, []) -> Graph;
add_vertices(Graph, [Head | Tail]) ->
  digraph:add_vertex(Graph, Head),
  add_vertices(Graph, Tail).


add_edges(Graph, []) -> Graph;
add_edges(Graph, [Head | Tail]) ->
  EdgeResult = digraph:edge(Graph, Head),
  {_, Vertex1, Vertex2, Label} = EdgeResult,
  digraph:add_edge(Graph, Vertex1, Vertex2, Label),
  add_edges(Graph, Tail).


process_concatenation([Second, First | Tail], State) ->
  Graph = merge(First#graph.nfa, Second#graph.nfa),
  digraph:add_edge(Graph, {vertex, First#graph.exit}, {vertex, Second#graph.entry}, eps),
  ConcatenationGraph = #graph{nfa   = Graph,
                              entry = First#graph.entry,
                              exit  = Second#graph.exit},
  {[ConcatenationGraph | Tail], State}.


process_alternation([Second, First | Tail], State) ->
  Graph = merge(First, Second),
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

%%
%%process_concatenation_test() ->
%%  [
%%    ?assertMatch({_, _, _}, process_concatenation(initialize_graph(), "abc", 2)),
%%    ?assertMatch({_, _, State} when State =:= 4, process_concatenation(initialize_graph(), "abc", 2)),
%%    ?assertMatch({_, Rest, _} when Rest =:= "c", process_concatenation(initialize_graph(), "abc", 2))
%%  ].
%%
%%process_alternation_test() ->
%%  [
%%    ?assertMatch({_, _, _}, process_alternation(initialize_graph(), "abc", 2)),
%%    ?assertMatch({_, _, State} when State =:= 5, process_alternation(initialize_graph(), "abc", 2)),
%%    ?assertMatch({_, Rest, _} when Rest =:= "c", process_alternation(initialize_graph(), "abc", 2))
%%  ].
%%
%%process_multiplication_test() ->
%%  [
%%    ?assertMatch({_, _, _}, process_multiplication(initialize_graph(), "abc", 2)),
%%    ?assertMatch({_, _, State} when State =:= 4, process_multiplication(initialize_graph(), "abc", 2)),
%%    ?assertMatch({_, Rest, _} when Rest =:= "bc", process_multiplication(initialize_graph(), "abc", 2))
%%  ].