%%%-------------------------------------------------------------------
%% @doc grepper top level supervisor.
%% @end
%%%-------------------------------------------------------------------

-module(grepper_sup).

-behaviour(supervisor).

%% API
-export([start_link/1]).

%% Supervisor callbacks
-export([init/1]).

-define(SERVER, ?MODULE).

%%====================================================================
%% API functions
%%====================================================================

start_link(FilePath) ->
  supervisor:start_link({local, ?SERVER}, ?MODULE, [FilePath]).

%%====================================================================
%% Supervisor callbacks
%%====================================================================

%% Child :: {Id,StartFunc,Restart,Shutdown,Type,Modules}
init(FilePath) ->
  TextSlices = file_slicer:slice(FilePath),
  Children = lists:map(fun create_child/1, TextSlices),
  {ok, {{one_for_all, 0, 1}, Children}}.

%%====================================================================
%% Internal functions
%%====================================================================

create_child(TextSlice) -> #{
  id => rand:uniform(),
  start => {graph_processor, process, [TextSlice]}
}.


