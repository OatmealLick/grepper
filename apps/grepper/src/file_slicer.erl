-module(file_slicer).
-include_lib("eunit/include/eunit.hrl").

%% API
-export([slice/1]).

process_count() -> 4.

slice(FilePath) ->
  {ok, File} = file:read_file(FilePath),
  doSlice(File).

doSlice(Text) -> doSlice(Text, process_count(), 0, string:length(Text), []).
doSlice(_, 0, _, _, TextPieces) -> lists:reverse(TextPieces);
doSlice(Text, ProcessesLeft, SliceStart, TextLength, TextPieces) ->
  SliceLength = TextLength div ProcessesLeft,
  [string:slice(Text, SliceStart, SliceLength) |
    doSlice(Text, ProcessesLeft - 1, SliceStart + SliceLength, TextLength - SliceLength, TextPieces)].

%% TEST
process_count_test() ->
  [
    ?assert(process_count() =:= 4) % :^)
  ].

slice_test() ->
  [
    ?assertMatch(L when length(L) == 4, doSlice("hello")),
    ?assertMatch(L when length(L) == 4, doSlice("hell")),
    ?assertMatch(["h", "e", "l", "l"], doSlice("hell")),
    ?assertMatch(["he", "ll", "ho", "und"], doSlice("hellhound")),
    ?assertMatch(["he", "ll", "hou", "nds"], doSlice("hellhounds"))
  ].
