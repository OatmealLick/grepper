-module(file_slicer).
-include_lib("eunit/include/eunit.hrl").

%% API
-export([slice/2]).

slice(FilePath, Parts) ->
  {ok, FileContent} = file:read_file(FilePath),
  do_slice(FileContent, Parts).

do_slice(Text, Parts) -> do_slice(Text, Parts, 0, string:length(Text), []).
do_slice(_, 0, _, _, TextPieces) -> lists:reverse(TextPieces);
do_slice(Text, ProcessesLeft, SliceStart, TextLength, TextPieces) ->
  SliceLength = TextLength div ProcessesLeft,
  [string:slice(Text, SliceStart, SliceLength) |
   do_slice(Text, ProcessesLeft - 1, SliceStart + SliceLength, TextLength - SliceLength, TextPieces)].

%% TEST

slice_test() ->
  [
    ?assertMatch(L when length(L) == 4, do_slice("hello", 4)),
    ?assertMatch(L when length(L) == 4, do_slice("hell", 4)),
    ?assertMatch(["h", "e", "l", "l"], do_slice("hell", 4)),
    ?assertMatch(["he", "ll", "ho", "und"], do_slice("hellhound", 4)),
    ?assertMatch(["he", "ll", "hou", "nds"], do_slice("hellhounds", 4))
  ].
