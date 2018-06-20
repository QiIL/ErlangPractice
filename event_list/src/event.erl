-module(event).
-compile(export_all).

-record(state, {server, name="", to_go=0}).

loop(S = #state{server=Server, to_go=[T|Next]}) ->
    receive
        {Server, Ref, cancel}->
            Server ! {Ref, ok}
    after T*1000 ->
            if Next =:= [] ->
                    Server ! {done, S#state.name};
                Next =/= [] ->
                    loop(S#state{to_go=Next})
            end
    end.

%% 由于Erlang受49天(49*24*60*60毫秒)的限制，
%% 因此需要使用这个函数
normalize(N) ->
    Litmit = 49 * 24 * 60 *60,
    [N rem Litmit | lists:duplicate(N div Litmit, Litmit)].

start(EventName, Delay) ->
    spawn(?MODULE, init, [self(), EventName, Delay]).

start_link(EventName, Delay) ->
    spawn_link(?MODULE, init, [self(), EventName, Delay]).

%%% 事件模块内部实现
init(Server, EventName, Delay) ->
    loop(#state{server=Server, name=EventName, to_go=normalize(Delay)}).

cancel(Pid) ->
    %% 设置监控器，以免进程已经死亡
    Ref = erlang:monitor(process, Pid),
    Pid ! {self(), Ref, cancel},
    receive
        {Ref, ok} -> 
            erlang:demonitor(Ref, [flush]),
            ok;
        {'DOWN', Ref, process, Pid, _Reason} ->
            ok
    end.