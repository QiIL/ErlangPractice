-module(linkmon).
-compile(export_all).

myproc() ->
    timer:sleep(5000),
    exit(reason).

chain(0) ->
    receive
        _ ->
            ok
    after 2000 ->
        exit("chain die here")
    end;
chain(N) ->
    Pid = spawn(fun() -> chain(N-1) end),
    link(Pid),
    receive
        _ ->
            ok
    end.

start_critic() ->
    spawn(?MODULE, critic, []).
start_critic2() ->
    spawn(?MODULE, restarter, []).

judge(Pid, Band, Album) ->
    Pid ! {self(), {Band, Album}},
    receive
        {Pid, Criticism} -> Criticism
    after 2000 ->
        timeout
    end.
judge2(Band, Album) ->
    Ref = make_ref(),
    critic ! {self(), Ref, {Band, Album}},
    receive
        {Ref, Criticism} -> Criticism
    after 2000 ->
        timeout
    end.

critic() ->
    receive
        {From, {"Rage", "Unit Testify"}} ->
            From ! {self(), "They are great"};
        {From, {"Sys", "Memoize"}} ->
            From ! {self(), "They're not Johnny Crash but they're good."};
        {From, {"Jon", "The Token Ring of Fire"}} ->
            From ! {self(), "Simply incredible"};
        {From, {_Band, _Album}} ->
            From ! {self(), "They are terrible"}
    end,
    critic().

critic2() ->
    receive
        {From, Ref, {"Rage", "Unit Testify"}} ->
            From ! {Ref, "They are great"};
        {From, Ref, {"Sys", "Memoize"}} ->
            From ! {Ref, "They're not Johnny Crash but they're good."};
        {From, Ref, {"Jon", "The Token Ring of Fire"}} ->
            From ! {Ref, "Simply incredible"};
        {From, Ref, {_Band, _Album}} ->
            From ! {Ref, "They are terrible"}
    end,
    critic2().

restarter() ->
    process_flag(trap_exit, true),
    Pid = spawn_link(?MODULE, critic2, []),
    register(critic, Pid),
    receive
        {'EXIT', Pid, normal} -> %正常死亡
            ok;
        {'EXIT', Pid, shutdown} -> %手动终止
            ok;
        {'EXIT', Pid, _} -> %非正常死亡
            restarter()
    end.