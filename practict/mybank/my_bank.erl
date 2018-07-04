-module(my_bank).
-export([
    start/0, stop/0, new_account/1, 
    deposit/2, withdraw/2, init/1, 
    handle_call/3, handle_cast/2, handle_info/2, 
    terminate/2, code_change/3
]).
-behaviour(gen_server).

start() -> gen_server:start_link({local, my_bank}, ?MODULE, [], []).
stop() -> gen_server:call(?MODULE, stop).

new_account(Who) -> gen_server:call(?MODULE, {new, Who}).
deposit(Who, Amount) -> gen_server:call(?MODULE, {add, Who, Amount}).
withdraw(Who, Amount) -> gen_server:call(?MODULE, {remove, Who, Amount}).

%% 回调函数
init([]) -> {ok, ets:new(?MODULE, [])}.

%% 同步调用
handle_call({new, Who}, _From, Tab) ->
    Reply = 
        case ets:lookup(Tab, Who) of
            [] -> ets:insert(Tab, {Who, 0}),
                {welcome, Who};
            [_] -> {Who , you_already_are_a_customer}
        end,
    {reply, Reply, Tab};
handle_call({add, Who, Amount}, _From, Tab) ->
    Reply = 
        case ets:lookup(Tab, Who) of
            [] -> {Who, you_are_not_out_customer};
            [{Who, OldAmount}] -> 
                NewBalance = OldAmount + Amount,
                ets:insert(Tab, {Who, NewBalance}),
                {thanks, Who, your_balance_is, NewBalance}
        end,
    {reply, Reply, Tab};
handle_call({remove, Who, Amount}, _From, Tab) ->
    Reply = 
        case ets:lookup(Tab, Who) of
            [] -> {Who, you_are_not_out_customer};
            [{Who, Balance}] when Amount =< Balance ->
                NewBalance = Balance - Amount,
                ets:insert(Tab, {Who, NewBalance}),
                {thanks, Who, your_balance_is, NewBalance};
            [{Who, Balance}] ->
                {sorry, Who, you_only_have, Balance, in_the_bank}
        end,
    {reply, Reply, Tab};
handle_call(stop, _From, Tab) ->
    {stop, normal, stopped, Tab}.

handle_cast(_Msg, State) -> {noreply, State}.
handle_info(_Info, State) -> {noreply, State}.
terminate(_Reason, _State) -> ok.
code_change(_OldVsn, State, _Extra) -> {ok, State}.
