-module(searcher).
-behaviour(gen_server).
-include("records.hrl").

%% API
-export([start_link/1]).

%% gen_server callbacks
-export([init/1,
         handle_call/3,
         handle_cast/2,
         handle_info/2,
         terminate/2,
         code_change/3]).

-record(state, {}).

%%%===================================================================
%%% API
%%%===================================================================

start_link(FilePart) ->
  gen_server:start_link(?MODULE, [FilePart], []).

%%%===================================================================
%%% gen_server callbacks
%%%===================================================================

init([FilePart]) ->
  self() ! {run, FilePart},
  {ok, #state{}}.

handle_call(_Request, _From, State) ->
  {reply, ok, State}.

handle_cast(_Request, State) ->
  {noreply, State}.

handle_info({run, FilePart}, State) ->
  Matcher = build_matcher(),
  graph_traversal:traverse(Matcher, FilePart),
  {noreply, State};

handle_info(_Info, State) ->
  {noreply, State}.

terminate(_Reason, _State) ->
  ok.

code_change(_OldVsn, State, _Extra) ->
  {ok, State}.

%%%===================================================================
%%% Internal functions
%%%===================================================================

build_matcher() ->
  {ok, Regex} = application:get_env(regex),
  G = post2nfa:convert(regex2post:convert(Regex)),
  #matcher{graph = G,
           current_state = G#graph.entry}.