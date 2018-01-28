-module(graph_processor).

%% API
-export([process/1]).

process(Text) ->
  io:fwrite("~p~n", [binary_to_list(Text)]),
  events:match_found(Text),
  timer:sleep(timer:seconds(rand:uniform(10))),
  events:finished().