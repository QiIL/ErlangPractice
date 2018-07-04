-module(kitty_gen_server).
-compile(export_all).
-record(cat, {name, color=green, desc}).
-behaviour(gen_server).

start_link() -> gen_server:start_link(?MODULE, [], []).

%% 同步调用
order_cat(Pid, Name, Color, Desc) ->
    gen_server:call(Pid, {order, Name, Color, Desc}).

%% 异步调用
return_cat(Pid, Cat = #cat{}) ->
    gen_server:cast(Pid, {return, Cat}).

%% 同步调用
close_shop(Pid) ->
    gen_server:call(Pid, terminate).


%%% 服务器函数
init([]) -> {ok, []}. % 此处无需做信息处理

handle_call({order, Name, Color, Desc}, _From, Cats) ->
    if Cats =:= [] ->
        {reply, make_cat(Name, Color, Desc), Cats};
        Cats =/= [] ->
            {reply, hd(Cats), tl(Cats)}
    end;

handle_call(terminate, _From, Cats) ->
    {stop, normal, ok, Cats}.

handle_cast({return, Cat = #cat{}}, Cats) ->
    {noreply, [Cat | Cats]}.

handle_info(Msg, Cats) ->
    io:format("Unexpected message: ~p~n", [Msg]),
    {noreply, Cats}.

terminate(normal, Cats) ->
    [io:format("~p was set free.~n", [C#cat.name]) || C <- Cats],
    ok.

code_change(_OldVsn, State, _Extra) ->
    {ok, State}.

make_cat(Name, Col, Desc) ->
    #cat{name=Name, color=Col, desc=Desc}.
