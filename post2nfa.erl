-module(post2nfa).
-include_lib("eunit/include/eunit.hrl").

%% API
-export([convert/1, merge/2, geta/0, getb/0, add_vertices/2]).


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
convert([], [{Head, _} | _], _) -> Head;
convert([Char | Input], Stack, CurrentState) ->
%%  io:fwrite("Char: "),
%%  io:write(Char),
%%  io:fwrite("   "),
%%  io:fwrite("Stack: "),
%%  io:write(Stack),
%%  io:fwrite("   "),
%%  io:fwrite("State: "),
%%  io:write(CurrentState),
%%  io:fwrite("\n"),
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
  {_, Vertex1, Vertex2, Label} = digraph:edge(Graph, Head),
  digraph:add_edge(Graph, Vertex1, Vertex2, Label),
  add_edges(Graph, Tail).


process_concatenation([{First, EntryState} | Tail], State) ->
  [{Second, _} | Rest] = Tail,
  Graph = merge(First, Second),
  {[{Graph, EntryState} | Rest], State}. %% TODO wtf state? can remove state from concat?


process_alternation([{First, FirstState} | Tail], State) ->
  [{Second, SecondState} | Rest] = Tail,
  Graph = merge(First, Second),
  StartVertex = digraph:add_vertex(Graph, {vertex, State + 1}),
  EndVertex = digraph:add_vertex(Graph, {vertex, State + 2}),
  FirstVertex = digraph:vertex(First, {vertex, FirstState}),
  SecondVertex = digraph:vertex(Second, {vertex, SecondState}),
  digraph:add_edge(Graph, StartVertex, FirstVertex, eps),
  digraph:add_edge(Graph, StartVertex, SecondVertex, eps),
  digraph:add_edge(Graph, FirstVertex, EndVertex, eps),
  digraph:add_edge(Graph, SecondVertex, EndVertex, eps),
  {[{Graph, State + 1} | Rest], State + 2}.


process_multiplication([{Head, EntryState} | Tail], State) ->
  NewVertex = digraph:add_vertex(Head, {vertex, State + 1}),
  HeadVertex = digraph:vertex(Head, {vertex, EntryState}),
  digraph:add_edge(Head, NewVertex, HeadVertex, eps),
  digraph:add_edge(Head, HeadVertex, NewVertex, eps),
  {[{Head, State + 1} | Tail], State + 1}.


process_literal(CharLiteral, Stack, State) ->
  GraphPiece = digraph:new(),
  StartVertex = digraph:add_vertex(GraphPiece, {vertex, State + 1}),
  EndVertex = digraph:add_vertex(GraphPiece, {vertex, State + 2}),
  digraph:add_edge(GraphPiece, StartVertex, EndVertex, CharLiteral),
  {[{GraphPiece, State} | Stack], State + 2}.


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