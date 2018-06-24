% hello.erl
-module(hello).
-export([say_hello/1]).

say_hello() -> io:format("hello~n").