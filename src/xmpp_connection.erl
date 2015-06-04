-module(xmpp_connection).
-export([start_link/4, init/4]).

-include_lib("exmpp/include/exmpp.hrl").
-include_lib("exmpp/include/exmpp_client.hrl").

start_link(JID, Host, Password, JIDs) ->
  {ok, spawn_link(?MODULE, init, [JID, Host, Password, JIDs])}.

init(JID, Host, Password, JIDs) ->
  % Send this message right away, so as soon as we get connected we'll have this in the mailbox
  self() ! send_messages,
  timer:send_interval(10000, send_messages),
  durable_connect(JID, Host, Password, JIDs).

% Connects and then goes into the receive loop
durable_connect(JID, Host, Password, JIDs) ->
  MySession = exmpp_session:start({1,0}),
  [User, Server] = string:tokens(JID, "@"),
  MyJID = exmpp_jid:make(User, Server, random),
  exmpp_session:auth_basic(MySession, MyJID, Password),
  try
    exmpp_session:connect_TCP(MySession, Host, 5222)
  catch throw:{socket_error, Error} ->
    error_logger:info_report({connection_failed, Host, Error}),
    timer:sleep(10000),
    durable_connect(JID, Host, Password, JIDs)
  end,
  error_logger:info_report({connection_established, Host}),
  exmpp_session:login(MySession, "PLAIN"),
  exmpp_session:send_packet(
    MySession,
    exmpp_presence:set_status(exmpp_presence:available(), "Fedex Bot")
  ),
  JIDsGraph = graph:complete_with_loops(JIDs),
  SendTo = graph:incident(JID, JIDsGraph, initial),
  ReceiveFrom = graph:incident(JID, JIDsGraph, terminal),
  loop(MySession, maps:from_list([{jid, JID}, {send_to, SendTo}, {receive_from, ReceiveFrom}, {last_send, 0}])).


send_message(MySession, JID) ->
  Message = exmpp_stanza:set_recipient(exmpp_message:chat("fedex"), JID),
  exmpp_session:send_packet(MySession, Message),
  ok.

loop(MySession, State) ->
  receive
    send_messages ->
      Now  = time:now(),
      Then = maps:get(last_send, State),
      if
        % send messages every 10 seconds roughly. This prevents us from
        % flooding XMPP messages if our mailbox fills up with send_messages
        % messages (say, if we cannot reconnect for a long time).
        Now - Then > 8 ->
          [ send_message(MySession, JID) || JID <- maps:get(send_to, State) ],
          loop(MySession, maps:update(last_send, time:now(), State));
        true ->
          % Not sending message.
          loop(MySession, State)
      end;
    #received_packet{packet_type=presence} ->
      % We do not care about presence. We can send messages to the other bot JIDs without
      % them being on our roster.
      loop(MySession, State);
    #received_packet{packet_type=message, from=From, type_attr="chat"} ->
      [TheirUser, TheirDomain, _Resource] = lists:map(fun(E) -> binary_to_list(E) end, tuple_to_list(From)),
      TheirJID = string:join([TheirUser, TheirDomain], "@"),
      case lists:any(fun(Allowed) -> Allowed == TheirJID end, maps:get(receive_from, State)) of
        true ->
          [_MyUser, MyDomain] = string:tokens(maps:get(jid, State), "@"),
          gen_server:cast(watchdog_server, {successful_delivery, TheirDomain, MyDomain});
        false ->
          error_logger:error_report({message_not_from_whitelist, maps:get(jid, State), TheirJID})
      end,
      loop(MySession, State);
    {stream_error, Error} ->
      error_logger:info_report({received_stream_error, Error, maps:get(jid, State)}),
      exmpp_session:stop(MySession),
      timer:sleep(5000);
      % Do not loop, let the process die
    Record = #received_packet{packet_type=message, type_attr="normal"} ->
      % Sometimes we get these messages from the server (on xmpp.jp for example) as a welcome or help message.
      % Log it but otherwise ignore it.
      error_logger:info_report({unexpected_normal_chat_message, Record}),
      loop(MySession, State);
    Record ->
      io:format("~nGot a message we don't expect: ~n~p", [Record]),
      loop(MySession, State)
  end.
