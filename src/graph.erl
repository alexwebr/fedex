% Functions for working with undirected graphs
% Everything is stored as an edge list
-module(graph).

-export([complete/1, complete_with_loops/1, edge/2, incident/3]).

complete(Vertices) ->
  lists:usort([ edge(X, Y) || X <- Vertices, Y <- Vertices, X =/= Y ]).

complete_with_loops(Vertices) ->
  Loops = lists:map(fun(V) -> edge(V, V) end, Vertices),
  lists:append(complete(Vertices), Loops).

% Returns all vertices incident to a vertex, but pretends all edges are
% directed and only returns those where Vertex is the initial or terminal
% vertex. Why?  It lets us divide all edges among all verticies without
% overlap.
incident(Vertex, Edges, Side) ->
  case Side of
    initial -> Us = 1, Them = 2;
    terminal -> Us = 2, Them = 1
  end,
  lists:filtermap(fun(Edge) ->
      if
        element(Us, Edge) == Vertex -> {true, element(Them, Edge)};
        true -> false
      end
    end,
    Edges).

% Used to represent edges in an undirected graph. X and Y are vertices.
edge(X, Y) when X > Y ->
  {X, Y};
edge(X, Y) ->
  {Y, X}.
