-module(distribute_test).
-compile(export_all).

start() -> register(dis_test, spawn(?MODULE, loop, [])).

loop() ->
    receive
        {Pid, stop} -> Pid ! "I die";
        {Pid, _Msg} -> Pid ! "you are a bab boy", loop()
    end.
