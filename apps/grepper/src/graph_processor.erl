-module(graph_processor).

%% API
-export([process/1]).

process(Text) ->
  io:fwrite(Text),
  oks.