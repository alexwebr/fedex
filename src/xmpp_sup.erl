-module(xmpp_sup).

-behaviour(supervisor).
%% API
-export([start_link/0]).

%% Supervisor callbacks
-export([init/1]).

%% ===================================================================
%% API functions
%% ===================================================================

start_link() ->
    {ok, Accounts} = application:get_env(accounts),
    supervisor:start_link({local, ?MODULE}, ?MODULE, Accounts).

%% ===================================================================
%% Supervisor callbacks
%% ===================================================================

init(Accounts) ->
    JIDs = lists:map(fun({JID, _, _}) -> JID end, Accounts),
    error_logger:info_report({"Starting XMPP connections", JIDs}),
    Children = lists:map(fun({JID, Host, Password}) ->
        {list_to_atom(JID),
          {xmpp_connection, start_link, [JID, Host, Password, JIDs]},
          permanent,
          5000,
          worker,
          [xmpp_connection]}
      end,
      Accounts),
    {ok, { {one_for_one, 2, 30}, Children }}.
