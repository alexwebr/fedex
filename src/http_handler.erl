-module(http_handler).
-export([get_graph/3, test/3]).

test(SessID, _Env, _In) ->
  mod_esi:deliver(SessID, [jiffy:encode({[{hello, woo}, {whats, up}]})]).

get_graph(SessID, _Env, _In) ->
  % Get the full list of graph edges, with associated "last successfully
  % delivered message" times.
  Edges = gen_server:call(watchdog_server, get_state),
  Now = time:now(),
  EdgeStatuses = lists:map(fun(Elem) ->
      {ServerA, ServerB} = element(1, Elem),
      Then = element(2, Elem),
      % IF the watchdog hasn't been kicked for 30 seconds, consider this s2s link down
      Down = Now - Then > 30,
      case ServerA == ServerB of
        true ->
          {[{type, local}, {server, list_to_binary(ServerA)}, {down, Down}]};
        false ->
          {[{type, s2s}, {servers, [list_to_binary(ServerA), list_to_binary(ServerB)]}, {down, Down}]}
      end
    end,
    maps:to_list(Edges)
  ),
  mod_esi:deliver(SessID, [jiffy:encode({[{edge_statuses, EdgeStatuses}]})]).
