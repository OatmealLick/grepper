-module(regex2tree).
-include_lib("eunit/include/eunit.hrl").

%% API
-export([re2post/1,
         extract_from_parentheses/1]).

re2post([]) -> [];
re2post(Input) -> re2post(Input, 0, [], []).

re2post([], WrittenCount, OperatorStack, Output) ->
  PostfixNotation = lists:append(lists:reverse(OperatorStack), Output),
  case (length(OperatorStack) =:= 0) and (WrittenCount > 1) of
    true -> Result = concatenation(PostfixNotation);
    false -> Result = PostfixNotation
  end,
  lists:reverse(Result);
re2post([Char | Input], WrittenCount, OperatorStack, Output) ->
  case is_unary_operator(Char) of
    true -> re2post(Input, WrittenCount, OperatorStack, [Char | Output]);
    false ->
      if
        WrittenCount =:= 2 ->
          case length(OperatorStack) > 0 of
            true ->
              [Operator | LeftOperators] = OperatorStack,
              re2post([Char | Input], 1, LeftOperators, [Operator | Output]);
            false ->
              re2post([Char | Input], 1, OperatorStack, concatenation(Output))
          end;
        true -> case Char of
            $( ->
              [Inside, Outside] = extract_from_parentheses(Input),
              PostfixOfInside = lists:reverse(re2post(Inside)),
              NewOutput = lists:append(PostfixOfInside, Output),
              re2post(Outside, WrittenCount + 1, OperatorStack, NewOutput);
            BinaryOperator when BinaryOperator =:= $+ ->
              re2post(Input, WrittenCount, [BinaryOperator | OperatorStack], Output);
            UnaryOperator when UnaryOperator =:= $* ->
              re2post(Input, WrittenCount, OperatorStack, [UnaryOperator | Output]);
            CharLiteral ->
              re2post(Input, WrittenCount + 1, OperatorStack, [CharLiteral | Output])
          end
      end
  end.

extract_from_parentheses(Input) -> extract_from_parentheses(Input, [], 0).

extract_from_parentheses([Char | Input], Inside, Count) ->
  case Char of
    $( -> extract_from_parentheses(Input, [Char | Inside], Count + 1);
    $) -> case Count of
             0 -> [lists:reverse(Inside), Input];
             _ -> extract_from_parentheses(Input, [Char | Inside], Count - 1)
           end;
    NotParentheses -> extract_from_parentheses(Input, [NotParentheses | Inside], Count)
  end.

is_unary_operator(Char) -> lists:member(Char, [$*]).

concatenation(Output) -> [$. | Output].


re2post_test() ->
  [
    ?assert(re2post("") =:= []),
    ?assert(re2post("a") =:= "a"),
    ?assert(re2post("(a)") =:= "a"),
    ?assert(re2post("((a))") =:= "a"),
    ?assert(re2post("aaa") =:= "aa.a."),
    ?assert(re2post("a(bb)") =:= "abb.."),
    ?assert(re2post("a+c") =:= "ac+"),
    ?assert(re2post("b+c+d") =:= "bc+d+"),
    ?assert(re2post("(aa)+(bb)") =:= "aa.bb.+"),
    ?assert(re2post("a(b+(cc)*)d(aa)*") =:= "abcc.*+.d.aa.*.")
  ].