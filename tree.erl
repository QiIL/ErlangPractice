-module(tree).
-compile([export_all]).

empty() -> {node, 'nil'}.

%% 二叉树
insert(Key, Val, {node, 'nil'}) ->
    {node, {Key, Val, {node, 'nil'}, {node, 'nil'}}};
insert(NewKey, NewVal, {node, {Key, Val, Smaller, Larger}}) when NewKey < Key ->
    {node, {Key, Val, insert(NewKey, NewVal, Smaller), Larger}};
insert(NewKey, NewVal, {node, {Key, Val, Smaller, Larger}}) when NewKey > Key ->
    {node, {Key, Val, Smaller, insert(NewKey, NewVal, Larger)}};
insert(Key, Val, {node, {Key, _, Smaller, Larger}}) ->
    {node, {Key, Val, Smaller, Larger}}.

look_up(_, {node, 'nil'}) ->
    undefine;
look_up(Key, {node, {Key, Val, _, _}}) ->
    {ok, Val};
look_up(Key, {node, {NodeKey, _, Smaller, _}}) when Key < NodeKey ->
    look_up(Key, Smaller);
look_up(Key, {node, {_, _, _, Larger}}) ->
    look_up(Key, Larger).

%% 斐波那契(Fibonacci)
fibonacci(0) -> 0;
fibonacci(1) -> 1;
fibonacci(N) -> fibonacci(N - 1) + fibonacci(N - 2).

fibonacci_1(N) -> fibonacci_1(N, 0, 1).
fibonacci_1(0, Ret1, _) -> Ret1;
fibonacci_1(N, Ret1, Ret2) -> fibonacci_1(N -1 , Ret2, Ret1 + Ret2).