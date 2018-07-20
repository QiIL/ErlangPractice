-module(eprof_test).
-compile(export_all).

start() ->
    Pid = spawn(?MODULE, loop, []),
    Ran = integer_to_list(rand:uniform(3000000)),
    Name = list_to_atom("eprof_test_" ++ Ran),
    register(Name, Pid),
    [Name, Pid].

loop() ->
    receive
        {From, stop} -> From ! "I die", cal();
        {From, Msg}  -> From ! "haha", nothing(), io:format("~p~n", [Msg]), loop()
    end.

cal() -> 1 + 1.
nothing() -> nothing.
