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
convert([], [{Head, _, _} | _], _) -> Head;
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


process_concatenation([{First, FirstEntryState, FirstExitState} | Tail], State) ->
  [{Second, SecondEntryState, SecondExitState} | Rest] = Tail,
  Graph = merge(First, Second),
  digraph:add_edge(Graph, digraph:vertex(First, {vertex, FirstExitState}),
    digraph:vertex(Second, {vertex, SecondEntryState}), eps),
  {[{Graph, FirstEntryState, SecondExitState} | Rest], State}.


process_alternation([{First, FirstEntryState, FirstExitState} | Tail], State) ->
  [{Second, SecondEntryState, SecondExitState} | Rest] = Tail,
  Graph = merge(First, Second),
  StartVertex = digraph:add_vertex(Graph, {vertex, State + 1}),
  EndVertex = digraph:add_vertex(Graph, {vertex, State + 2}),
  FirstEntryVertex = digraph:vertex(First, {vertex, FirstEntryState}),
  FirstExitVertex = digraph:vertex(First, {vertex, FirstExitState}),
  SecondEntryVertex = digraph:vertex(Second, {vertex, SecondEntryState}),
  SecondExitVertex = digraph:vertex(Second, {vertex, SecondExitState}),
  digraph:add_edge(Graph, StartVertex, FirstEntryVertex, eps),
  digraph:add_edge(Graph, StartVertex, SecondEntryVertex, eps),
  digraph:add_edge(Graph, FirstExitVertex, EndVertex, eps),
  digraph:add_edge(Graph, SecondExitVertex, EndVertex, eps),
  {[{Graph, State + 1, State + 2} | Rest], State + 2}.


process_multiplication([{Head, EntryState, ExitState} | Tail], State) ->
  NewVertex = digraph:add_vertex(Head, {vertex, State + 1}),
  HeadEntryVertex = digraph:vertex(Head, {vertex, EntryState}),
  HeadExitVertex = digraph:vertex(Head, {vertex, ExitState}),
  digraph:add_edge(Head, NewVertex, HeadEntryVertex, eps),
  digraph:add_edge(Head, HeadExitVertex, NewVertex, eps),
  {[{Head, State + 1, State + 1} | Tail], State + 1}.


process_literal(CharLiteral, Stack, State) ->
  Graph = digraph:new(),
  StartVertex = digraph:add_vertex(Graph, {vertex, State + 1}),
  EndVertex = digraph:add_vertex(Graph, {vertex, State + 2}),
  digraph:add_edge(Graph, StartVertex, EndVertex, CharLiteral),
  {[{Graph, State + 1, State + 2} | Stack], State + 2}.


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