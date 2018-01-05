-module(text_dispatcher).
-include_lib("eunit/include/eunit.hrl").

%% API
-export([dispatch/1]).

process_count() -> 4.

dispatch([]) -> empty_text;
dispatch(Text) ->
  TextPieces = slice(Text),
  lists:map(fun({TextPiece}) -> spawn(graph_processor, process, [TextPiece]) end, TextPieces).

slice(Text) -> slice(Text, process_count(), 0, string:length(Text), []).
slice(_, 0, _, _, TextPieces) -> lists:reverse(TextPieces);
slice(Text, ProcessesLeft, SliceStart, TextLength, TextPieces) ->
  SliceLength = TextLength div ProcessesLeft,
  [string:slice(Text, SliceStart, SliceLength) |
    slice(Text, ProcessesLeft - 1, SliceStart + SliceLength, TextLength - SliceLength, TextPieces)].

%% TEST
process_count_test() ->
  [
    ?assert(process_count() =:= 4) % :^)
  ].

slice_test() ->
  [
    ?assertMatch(L when length(L) == 4, slice("hello")),
    ?assertMatch(L when length(L) == 4, slice("hell")),
    ?assertMatch(["h", "e", "l", "l"], slice("hell")),
    ?assertMatch(["he", "ll", "ho", "und"], slice("hellhound")),
    ?assertMatch(["he", "ll", "hou", "nds"], slice("hellhounds"))
  ].