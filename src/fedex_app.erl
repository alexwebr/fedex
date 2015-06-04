-module(fedex_app).

-behaviour(application).

%% Application callbacks
-export([start/0, start/2, stop/1]).

% Used when starting the app from debug mode like this:
%   erl -pa ebin/ -boot start_sasl -s fedex
start() ->
  application:ensure_all_started(fedex),

  Port = 8099,
  inets:start(httpd, [
    {port, Port},
    {server_name, "localhost"},
    {document_root, "webroot"},
    {modules,[mod_esi, mod_get]},
    {server_root, "."},
    {erl_script_alias, {"/esi", [http_handler]}}
  ]),
  error_logger:info_report("Fedex started.").

%% ===================================================================
%% Application callbacks
%% ===================================================================

start(_StartType, _StartArgs) ->
    fedex_sup:start_link().

stop(_State) ->
    ok.
