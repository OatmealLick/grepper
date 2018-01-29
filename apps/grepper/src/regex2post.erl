-module(regex2post).
-include_lib("eunit/include/eunit.hrl").

%% API
-export([convert/1]).

convert([]) -> [];
convert(Input) -> convert(Input, 0, [], []).


convert([], WrittenCount, OperatorStack, Output) ->
  PostfixNotation = lists:append(lists:reverse(OperatorStack), Output),
  case (length(OperatorStack) =:= 0) and (WrittenCount > 1) of
    true -> Result = concatenation(PostfixNotation);
    false -> Result = PostfixNotation
  end,
  lists:reverse(Result);
convert([Char | Input], WrittenCount, OperatorStack, Output) ->
  case is_unary_operator(Char) of
    true -> convert(Input, WrittenCount, OperatorStack, [Char | Output]);
    false ->
      case WrittenCount =:= 2 of
        true ->
          {LeftOperators, NewOutput} = append_operator(OperatorStack, Output),
          convert([Char | Input], 1, LeftOperators, NewOutput);
        false ->
          {LeftInput, NewWrittenCount, NewOperatorStack, NewOutput} =
          consume_next_char([Char | Input], WrittenCount, OperatorStack, Output),
          convert(LeftInput, NewWrittenCount, NewOperatorStack, NewOutput)
      end
  end.


append_operator(OperatorStack, Output) ->
  case length(OperatorStack) > 0 of
    true ->
      [Operator | LeftOperators] = OperatorStack,
      {LeftOperators, [Operator | Output]};
    false ->
      {OperatorStack, concatenation(Output)}
  end.


consume_next_char([Char | Input], WrittenCount, OperatorStack, Output) ->
  case Char of
    $( ->
      [Inside, Outside] = extract_from_parentheses(Input),
      PostfixOfInside = lists:reverse(convert(Inside)),
      NewOutput = lists:append(PostfixOfInside, Output),
      {Outside, WrittenCount + 1, OperatorStack, NewOutput};
    BinaryOperator when BinaryOperator =:= $+ ->
      {Input, WrittenCount, [BinaryOperator | OperatorStack], Output};
    UnaryOperator when UnaryOperator =:= $* ->
      {Input, WrittenCount, OperatorStack, [UnaryOperator | Output]};
    CharLiteral ->
      {Input, WrittenCount + 1, OperatorStack, [CharLiteral | Output]}
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


%% TEST

re2post_test() ->
  [
    ?assert(convert("") =:= []),
    ?assert(convert("a") =:= "a"),
    ?assert(convert("(a)") =:= "a"),
    ?assert(convert("((a))") =:= "a"),
    ?assert(convert("aaa") =:= "aa.a."),
    ?assert(convert("a(bb)") =:= "abb.."),
    ?assert(convert("a+c") =:= "ac+"),
    ?assert(convert("b+c+d") =:= "bc+d+"),
    ?assert(convert("(aa)+(bb)") =:= "aa.bb.+"),
    ?assert(convert("a(b+(cc)*)d(aa)*") =:= "abcc.*+.d.aa.*.")
  ].