-module(grepper_app).
-behaviour(application).

-export([start/2, stop/1]).

%% @doc
%% Grepper application behaviour, initializes top level supervisor.
start(normal, _StartArgs) ->
  grepper_sup:start_link().


stop(_State) ->
  ok.
