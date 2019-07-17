%%%-------------------------------------------------------------------
%%% @author MaorAssayag
%%% @copyright (C) 2019, <COMPANY>
%%% @doc
%%% @end
%%% Created : 24.05.2019
%%%-------------------------------------------------------------------
-module(ex6_318550746).
-author("MaorAssayag").
-export([songList/1, songGen/3]).

%% songList(Songs)
% Constructs a data structure that stores all the songs given in list Songs
% Songs is a list of songs, where each song is a string
% Data structure must be constructed using the digraph: library
% songList/1 should print the screen the number of edges created in G
% Empty song list may be given
songList(Songs) ->
  % Create digraph object
  G = digraph:new(),
  % Add Songs to G
  addSongs(G, Songs),
  % Create the graph edges
  connectVertexes(G),
  % Promote number of edges in G
  io:format("Number of edges created in G: ~p~n", [digraph:no_edges(G)]),
  % return G
  G.

%% songGen(G, Start, End)
% Returns the shortest song list that starts with song Start and ends with song End
% The first letter of a song must be the same as the last one of the previous song
% G is an output of songList/1
% digraph:get_short_path return the atom false if no path has been found - as required
songGen(G, Start, End) -> digraph:get_short_path(G, Start, End).

%% addSongs(G,Songs)
% Add Songs to G graph
addSongs(G, Songs) -> [digraph:add_vertex(G, Song) || Song <- Songs].

%% connectVertexes(G)
% Add to G the edges that containes a Song V1 with a last letter equal to the first letter of V2
connectVertexes(G) ->
  % Create all the edges that containes a Song V1 with a last letter equal to the first letter of V2
  Edges = [ {V1,V2} || V1 <- digraph:vertices(G), V2 = [H|_] <- digraph:vertices(G), lists:last(V1) =:= H, V1 =/= V2],
  [digraph:add_edge(G,V1,V2) || {V1,V2} <- Edges].
