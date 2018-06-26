-module(kitty_server).
-compile(export_all).

-record(cat, {name, color=brown, desc}).

%%% 客户api
start_link() ->
    my_server:start_link(?MODULE, []).

%% 同步调用
order_cat(Pid, Name, Color, Desc) ->
    my_server:call(Pid, {order, Name, Color, Desc}).

close_shop(Pid) ->
    my_server:call(Pid, terminate).

%% 异步调用
return_cat(Pid, Cat = #cat{}) ->
    my_server:cast(Pid, {return, Cat}).

init([]) -> [].

handle_call({order, Name, Color, Desc}, From, Cats) ->
    if Cats =:= [] ->
            my_server:reply(From, make_cat(Name, Color, Desc)),
            Cats;
        Cats =/= [] ->
            my_server:reply(From, hd(Cats)),
            tl(Cats)
    end;
handle_call(terminate, From, Cats) ->
    my_server:reply(From, ok),
    terminate(Cats).

handle_cast({return, Cat = #cat{}}, Cats) ->
    [Cat | Cats].

make_cat(Name, Color, Desc) ->
    #cat{name=Name, color=Color, desc=Desc}.

terminate(Cats) ->
    [io:format("~p is free now~n", [Cat]) || Cat <- Cats],
    exit(normal).