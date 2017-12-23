-module(regex2tree).
-include_lib("eunit/include/eunit.hrl").

%% API
-export([re2post/1, extract_from_parentheses/1]).

re2post(Input) -> re2post(Input, []).

re2post([], Output) -> Output;
re2post(Input, Output) ->
  case string:slice(Input, 0, 1) of
    "+" -> re2post(string:slice(Input, 1, string:length(Input)), lists:append(Output, ["+","."]));
    "*" -> re2post(string:slice(Input, 1, string:length(Input)), lists:append(Output, ["*","."]));
    "(" -> InsideParentheses = extract_from_parentheses(Input),
      PostfixOfInside = lists:append(re2post(InsideParentheses), ["."]),
      re2post(string:slice(Input, string:length(InsideParentheses) + 2), lists:merge(Output, PostfixOfInside));
    CharLiteral -> re2post(string:slice(Input, 1, string:length(Input)), lists:append(Output, [CharLiteral]))
end.

extract_from_parentheses(String) -> [Inside|_] = string:split(string:slice(String, 1), ")", trailing), Inside.


re2post_test() ->
  [
    ?assert(re2post("") =:= []),
    ?assert(re2post("a") =:= ["a"]),
    ?assert(re2post("a(bb)") =:= ["a","b","b","."]),
    ?assert(re2post("a+c") =:= ["a","+",".","c"])
  ].

extract_from_parentheses_test() ->
  [
    ?_assert(extract_from_parentheses("()") =:= ""),
    ?_assert(extract_from_parentheses("(a)") =:= "a"),
    ?_assert(extract_from_parentheses("(ab)") =:= "ab"),
    ?_assert(extract_from_parentheses("(asdf)") =:= "asdf"),
    ?_assert(extract_from_parentheses("((dupa)a)") =:= "(dupa)a")
  ].