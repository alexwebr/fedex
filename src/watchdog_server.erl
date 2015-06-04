-module(watchdog_server).
-behaviour(gen_server).
-export([start_link/0, init/1, handle_call/3, handle_cast/2, handle_info/2,
     terminate/2, code_change/3]).

start_link() ->
  {ok, Accounts} = application:get_env(accounts),
  Servers = lists:map(fun({JID, _Host, _Password}) ->
      [_Username, Domain] = string:tokens(JID, "@"),
      Domain
    end,
    Accounts),
  gen_server:start_link({local, ?MODULE}, ?MODULE, Servers, []).

% We get a list of servers. We want to keep track of the "most recently
% delivered message" for each server-to-server pair, and also for local
% delivery on each server (two JIDs on the same domain).
% In graph terms, we make an "undirected complete graph" with a vertex for each
% server, and add a single "loop" for each server to represent local delivery.
% We store this graph as an edge list.  We have a hash table, where each key is
% an edge and the values are "timestamp of last delivered message"
init(Servers) ->
  {ok, maps:from_list([ {Edge, 0} || Edge <- graph:complete_with_loops(Servers) ])}.

handle_cast({successful_delivery, ServerA, ServerB}, State) ->
  NowSecs = time:now(),
  {noreply, maps:update(graph:edge(ServerA, ServerB), NowSecs, State)}.

% Used if we want to poll for changes
handle_call(get_state, _From, State) ->
  {reply, State, State}.

handle_info(_Info, State) ->
    {noreply, State}.

terminate(_Reason, _State) ->
    ok.

code_change(_OldVsn, State, _Extra) ->
    {ok, State}.
