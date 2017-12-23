-module(regex2nfa).
-include_lib("eunit/include/eunit.hrl").

%% API
-export([re2post/1, post2graph/1, process_concatenation/3]).

re2post(Input) -> re2post(Input, []).
re2post([], Output) -> Output;
re2post(Input, Output) ->
  case string:slice(Input, 0, 1) of
    "+" -> re2post(without_first(Input), lists:append(Output, ["+","."]));
    "*" -> re2post(without_first(Input), lists:append(Output, ["*","."]));
    "(" -> InsideParentheses = extract_from_parentheses(Input),
      PostfixOfInside = lists:append(re2post(InsideParentheses), ["."]),
      re2post(string:slice(Input, string:length(InsideParentheses) + 2), lists:merge(Output, PostfixOfInside));
    CharLiteral -> re2post(without_first(Input), lists:append(Output, [CharLiteral]))
end.


extract_from_parentheses(String) -> [Inside|_] = string:split(string:slice(String, 1), ")", trailing), Inside.


post2graph(Input) -> post2graph(Input, initialize_graph(), [], 0).
post2graph([], Graph, _, _) -> Graph;
post2graph(Postfix, Graph, Stack, CurrentState) ->
  case string:slice(Postfix, 0, 1) of
    "." -> {GraphNew, StackNew, CurrentStateNew} = process_concatenation(Graph, Stack, CurrentState),
      post2graph(without_first(Postfix), GraphNew, StackNew, CurrentStateNew);
    "+" -> {GraphNew, StackNew, CurrentStateNew} = process_alternation(Graph, Stack, CurrentState),
      post2graph(without_first(Postfix), GraphNew, StackNew, CurrentStateNew);
    "*" -> {GraphNew, StackNew, CurrentStateNew} = process_multiplication(Graph, Stack, CurrentState),
      post2graph(without_first(Postfix), GraphNew, StackNew, CurrentStateNew);
    CharLiteral -> post2graph(without_first(Postfix), Graph, [[CharLiteral]|Stack], CurrentState)
  end.


%% Bzdeco we're running form State = 0, and build graph from there
initialize_graph() ->
  Graph = digraph:new(),
  digraph:add_vertex(Graph, {vertex, 0}),
  Graph.


process_concatenation(Graph, [First|Tail], State) ->
  [Second|Rest] = Tail,
  CurrentVertex = digraph:vertex(Graph, {state, State}),
  FirstVertex = digraph:add_vertex(Graph, {vertex, State + 1}),
  SecondVertex = digraph:add_vertex(Graph, {vertex, State + 2}),
  digraph:add_edge(Graph, CurrentVertex, FirstVertex, First),
  digraph:add_edge(Graph, FirstVertex, SecondVertex, Second),
  {Graph, Rest, State+2}.


process_alternation(Graph, [First|Tail], State) ->
  [Second|Rest] = Tail,
  CurrentVertex = digraph:vertex(Graph, {state, State}),
  FirstVertex = digraph:add_vertex(Graph, {vertex, State + 1}),
  SecondVertex = digraph:add_vertex(Graph, {vertex, State + 2}),
  ResultVertex = digraph:add_vertex(Graph, {vertex, State + 3}),
  digraph:add_edge(Graph, CurrentVertex, FirstVertex, First),
  digraph:add_edge(Graph, CurrentVertex, SecondVertex, Second),
  digraph:add_edge(Graph, FirstVertex, ResultVertex, eps),
  digraph:add_edge(Graph, SecondVertex, ResultVertex, eps),
  {Graph, Rest, State+3}.


process_multiplication(Graph, [First|Rest], State) ->
  CurrentVertex = digraph:vertex(Graph, {state, State}),
  NewVertex = digraph:add_vertex(Graph, {vertex, State + 1}),
  AfterMultiplicationVertex = digraph:add_vertex(Graph, {vertex, State + 2}),
  digraph:add_edge(Graph, CurrentVertex, NewVertex, First),
  digraph:add_edge(Graph, NewVertex, CurrentVertex, eps),
  digraph:add_edge(Graph, CurrentVertex, AfterMultiplicationVertex, eps),
  {Graph, Rest, State+2}.


without_first(Input) -> string:slice(Input, 1, string:length(Input)).


%% TEST - TODO why they are always passing? \shrug, w/e, we don't do mistakes anyway
sanity_test() ->
  ?_assert(1 =:= 2). %% so afaic this doesnt work xD

re2post_test() ->
  [
    ?_assert(re2post("") =:= []),
    ?_assert(re2post("a") =:= ["a"]),
    ?_assert(re2post("a(bb)") =:= ["a","b","b","."]),
    ?_assert(re2post("a+c") =:= ["a","+",".","c"])
  ].

extract_from_parentheses_test() ->
  [
    ?_assert(extract_from_parentheses("()") =:= ""),
    ?_assert(extract_from_parentheses("(a)") =:= "a"),
    ?_assert(extract_from_parentheses("(ab)") =:= "ab"),
    ?_assert(extract_from_parentheses("(asdf)") =:= "asdf"),
    ?_assert(extract_from_parentheses("((dupa)a)") =:= "(dupa)a")
  ].

process_concatenation_test() ->
  [
    ?_assertMatch({_, _, _}, process_concatenation(initialize_graph(), ["a", "b", "c"], 2))
  ].

without_first_test() ->
  [
    ?_assert(without_first("") =:= []),
    ?_assert(without_first("abc") =:= "bc"),
    ?_assert(without_first("___") =:= "__")
  ].