-module(graph_traversal).

%% API
-export([]).

%% @doc
%% Feeds compiled NFA graph with given text and searches for matches.
%% Whenever there is more than one edge emanating from current state labeled with eps new process(es) are created,
%% one for each edge.
%% @param Graph regex compiled into NFA graph
%% @param Text text which chars will feed the graph traversing process
%% @end
traverse(GraphR, Text) ->


