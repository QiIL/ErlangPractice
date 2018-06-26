-module(kitty).
-compile(export_all).

-record(cat, {name, color=green, description}).

%%% 客户端api
start_link() -> spawn_link(fun init/0).

%%% 同步调用
order_cat(Pid, Name, Color, Description) ->
    kitty:call(Pid, {order, Name, Color, Description}).

%% 这个调用是异步的
return_cat(Pid, Cat = #cat{}) ->
    Pid ! {return, Cat},
    ok.

%% 同步调用
close_shop(Pid) ->
    kitty:call(Pid, terminate).

%%% 服务器函数
init() -> loop([]).

loop(Cats) ->
    receive
        {Pid, Ref, {order, Name, Color, Description}} ->
            if Cats =:= [] ->
                Pid ! {Ref, make_cat(Name, Color, Description)},
                loop(Cats);
                Cats =/= [] -> %减少库存
                Pid ! {Ref, hd(Cats)},
                loop(tl(Cats))
            end;
        {return, Cat = #cat{}} ->
            loop([Cat| Cats]);
        {Pid, Ref, terminate} ->
            Pid ! {Ref, ok},
            terminate(Cats);
        Unkonwn ->
            %% 做些日志
            io:format("Unkonwn message: ~p~n", [Unkonwn]),
            loop(Cats)
    end.

%%% 私有函数
make_cat(Name, Col, Desc) ->
    #cat{name=Name, color=Col, description=Desc}.

terminate(Cats) ->
    [io:format("~p was set free.~n", [C#cat.name]) || C <-Cats],
    ok.

call(Pid, Msg) ->
    Ref = erlang:monitor(process, Pid),
    Pid ! {Ref, Msg},
    receive
        {Ref, Reply} ->
            erlang:demonitor(Ref, [flush]),
            Reply;
        {'DOWN', Ref, process, Pid, Reason} ->
            erlang:error(Reason)
    after 5000 ->
        erlang:error(timeout)
    end.
