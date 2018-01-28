-module(grepper_app).
-behaviour(application).

-export([start/2, stop/1]).

start(normal, _StartArgs) ->
  grepper_sup:start_link().

stop(_State) ->
  ok.
