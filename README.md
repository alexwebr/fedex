Fedex
=====

![Screenshot of Fedex in the browser](/screenshot.png?raw=true "Screenshot")

Fedex is a daemon that monitors multiple Jabber/XMPP domains. It covers two things:
 * Local delivery is working (a user sending to themselves)
 * Delivery between domains (delivery from each domain to every other configured domain)

It is written in Erlang and uses [exmpp by ProcessOne][exmpp] for its XMPP
connections.  It is still very basic; it only serves a webpage with the status
of all domains as a [Cytoscape.js][cyto] graph, suitable for use in a NOC
wallboard (see the above screenshot).

In the future it should have:
 * An alerting system (SMS, email, etc.)
 * Logging events ("what time did the link go down?")
 * Automated tests! (I'm still learning Erlang)

Using
-----

1. First, you need accounts on the XMPP domains you want to monitor.
1. Then, edit `src/fedex.app.src` and add those accounts (JID, hostname, password).
1. Finally, build and run with: `erl -make && ./start.sh && tail -f fedex.log`
1. Browse to http://hostname:8099/index.html
1. That's it!

Bugs
----

Bugs are tracked in BugsEverywhere. Install it and do "be list" to see the list.

[exmpp]: https://github.com/processone/exmpp
[cyto]: http://js.cytoscape.org/
