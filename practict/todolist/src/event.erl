-module(event).
-compile(export_all).
-record(state, {server, name="", to_go=0}).

loop(S = #state{server=Server, to_go=[T|Next]}) ->
    receive
        {Server, Ref, cancel} ->
            Server ! {Ref, ok}
    after T * 1000 ->
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
    
start(EventName, DateTime) ->
    spawn(?MODULE, init, [self(), EventName, DateTime]).

start_link(EventName, DateTime) ->
    spawn_link(?MODULE, init, [self(), EventName, DateTime]).

init(Server, EventName, DateTime) ->
    loop(#state{server=Server, name=EventName, to_go=time_to_go(DateTime)}).

cancel(Pid) ->
    %% 设置监控起，以免进程死亡
    Ref = erlang:monitor(process, Pid),
    Pid ! {self(), Ref, cancel},
    receive
        {Ref, ok} ->
            erlang:demonitor(Ref, [flush]),
            ok;
        {'DOWN', Ref, process, Pid, _Reason} ->
            ok
    end.

time_to_go(TimeOut={{_, _, _}, {_, _, _}}) ->
    Now = calendar:local_time(),
    ToGo = calendar:datetime_to_gregorian_seconds(TimeOut) -
        calendar:datetime_to_gregorian_seconds(Now),
    Secs = if ToGo > 0 -> ToGo;
                ToGo < 0 -> 0
            end,
    normalize(Secs).
