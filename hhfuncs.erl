-module(hhfuncs).
-compile([export_all]).

one() -> 1.
two() -> 2.

add(X, Y) -> X() + Y().

increment([]) -> [];
increment([H | T]) -> [H + 1 | increment(T)].

decrement([]) -> [];
decrement([H | T]) -> [H - 1 | decrement(T)].

increment_1(List) -> lists:reverse(increment_1(List, [])).
increment_1([], List) -> List;
increment_1([H | T], List) -> increment_1(T, [H + 1 | List]).

map(F, List) -> lists:reverse(map(F, List, [])).
map(_, [], List) -> List;
map(F, [H | T], List) -> map(F, T, [F(H) | List]).

incr(X) -> X + 2.
decr(X) -> X - 2.

%% 作用域和闭包
base(A) ->
    B = A + 1,
    F = fun() -> A * B end,
    F().

%% 超出闭包作用域
% base2(A) ->
%     B = A + 1,
%     F = fun
%         () ->
%             C = A + B
%     end,
%     F(),
%     C.

a() ->
    Secret = "pony",
    fun() -> Secret end.

b(F) ->
    "a/0's password is " ++ F().

%% 对比父级作用域的变量报错样例
% base2() -> 
%     A = 1,
%     (fun() -> A = 2 end)().

%% 成功定义一个与父作用域有相同名字的变量
% base3() ->
%     A = 1,
%     (fun(A) -> A = 2 end)(2).

%% 挑选偶数
even(L) -> lists:reverse(even(L, [])).
even([], Acc) -> Acc;
even([H | T], Acc) when H rem 2 == 0 ->
    even(T, [H | Acc]);
even([_ | T], Acc) ->
    even(T, Acc).

%% 挑选老人
old_man(L) -> lists:reverse(old_man(L, [])).
old_man([], Acc) -> Acc;
old_man([H | T], Acc) when H > 60 ->
    old_man(T, [H | Acc]);
old_man([_ | T], Acc) ->
    old_man(T, Acc).

%% 遍历列表然后对每个元素做相同的操作
filter(Pred, L) -> lists:reverse(filter(Pred, L, [])).
filter(_, [], Acc) -> Acc;
filter(Pred, [H | T], Acc) ->
    case Pred(H) of
        true -> filter(Pred, T, [H | Acc]);
        false -> filter(Pred, T, Acc)
    end.

%% 折叠列表函数
fold(_, Start, []) -> Start;
fold(F, Start, [H | T]) -> fold(F, F(H, Start), T).

%% 利用折叠函数
%% 反转
reverse(L) ->
    fold(fun(X, Acc) -> [X | Acc] end, [], L).
%% 遍历
map2(F, L) ->
    reverse(fold(fun(X, Acc) -> [F(X) | Acc] end, [], L)).
%% 筛选
filter2(Pred, L) ->
    F = fun(X, Acc) -> 
        case Pred(X) of
            true -> [X | Acc];
            false -> Acc
        end
    end,
    reverse(fold(F, [], L)).

