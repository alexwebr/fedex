-module(time).
-export([now/0]).

now() ->
  element(2, os:timestamp()).
