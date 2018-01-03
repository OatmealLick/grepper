-module(text_dispatcher).
-include_lib("eunit/include/eunit.hrl").

%% API
-export([dispatch/1]).

process_count() -> 4.

dispatch([]) -> empty_text;
dispatch(Text) -> dispatch(Text, process_count(), 0, string:length(Text), []).
dispatch(_, 0, _, _, PidList) -> lists:reverse(PidList);
dispatch(Text, ProcessesLeft, SliceStart, SliceLength, PidList) ->
  Pid = spawn(graph_processor, process, [string:slice(Text, SliceStart, SliceLength)]),
  dispatch(Text, ProcessesLeft - 1, SliceLength, SliceStart + SliceLength, [Pid|PidList]).

%% TEST
process_count_test() ->
  [
    ?assert(process_count() =:= 4) % :^)
  ].