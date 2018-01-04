-module(graph_processor).

%% API
-export([process/1]).

process(Text) ->
  timer:sleep(timer:seconds(rand:uniform(8))),
  io:write(self()),
  io:write(Text),
  ok.