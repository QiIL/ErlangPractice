-module(recursion).
-compile([export_all]).

%% 求列表长度
list_length([]) -> 0;
list_length([_ | T]) -> 1 + list_length(T).

%% case版本
len(List, Total) ->
    case List of
        [] -> Total;
        [_ | T] -> len(T, Total + 1)
    end.

%% 尾递归版本
len2(N) -> len2(N, 0).
len2([], Total) -> Total;
len2([_ | T], Total) -> len2(T, Total + 1).

%% 复制N份传入的东西
duplicate(0, _) -> [];
duplicate(N, Term) when N > 0 ->
    [Term | duplicate(N-1, Term)].
%% 尾递归版本。
duplicate2(N, List) -> duplicate2(N, List, []).
duplicate2(0, _, List) -> List;
duplicate2(N, Term, List) when N > 0 ->
    duplicate2(N-1, Term, [Term|List]).

%% 反转列表
reverse([]) -> [];
reverse([H | T]) -> reverse(T) ++ [H].
%% 尾递归版本
reverse2(List) -> reverse2(List, []).
reverse2([], Acc) -> Acc;
reverse2([H | T], Acc) -> reverse2(T, [H | Acc]).

%% 求子列表
sublist(_, 0) -> [];
sublist([], _) -> [];
sublist([H | T], N) when N > 0 -> [H | sublist(T, N - 1)].
%% 尾递归
sublist2(List, N) -> sublist2(List, N, []).
sublist2(_, 0, SubList) -> SubList;
sublist2([], _, SubList) -> SubList;
sublist2([H | T], N, SubList) when N > 0 -> sublist2(T, N-1, SubList ++ [H]).

%% 拼合函数
zip([], []) -> [];
zip([X | Xrest], [Y | Yrest]) -> [{X, Y} | zip(Xrest, Yrest)].
%% 非限制长度相同的版本
lenient_zip([], _) -> [];
lenient_zip(_, []) -> [];
lenient_zip([X | Xrest], [Y | Yrest]) -> [{X, Y} | lenient_zip(Xrest, Yrest)].
%% 尾递归
zip2(Left, Right) -> zip2(Left, Right, []).
zip2([], [], Rest) -> Rest;
zip2([Lh | Lt], [Rh | Rt], Rest) -> zip2(Lt, Rt, Rest ++ [{Lh, Rh}]).
lenient_zip2(Left, Right) -> lenient_zip2(Left, Right, []).
lenient_zip2([], _, Rest) -> Rest;
lenient_zip2(_, [], Rest) -> Rest;
lenient_zip2([Lh | Lt], [Rh | Rt], Rest) -> lenient_zip2(Lt, Rt, Rest ++ [{Lh, Rh}]).

%% 快排
quick_sort([]) -> [];
quick_sort([Pivot | Rest]) ->
    {Smaller, Larger} = separate(Pivot, Rest, [], []),
    quick_sort(Smaller) ++ [Pivot] ++ quick_sort(Larger).

%% 分割函数
separate(_, [], Smaller, Larger) -> {Smaller, Larger};
separate(Pivot, [H | T], Smaller, Larger) -> 
    case Pivot < H of
        true -> separate(Pivot, T, [H | Smaller], Larger);
        false -> separate(Pivot, T, Smaller, [H | Larger])
    end.
    
%% 模式匹配写法
lc_quick_sort([]) -> [];
lc_quick_sort([Pivot | Rest]) ->
    lc_quick_sort([Smaller || Smaller <- Rest, Smaller < Pivot])
    ++ [Pivot] ++ 
    lc_quick_sort([Larger || Larger <- Rest, Larger >= Pivot]).

%% 返回三个列表版本
bestest_qsort([]) -> [];
bestest_qsort(L=[_|_]) ->
    bestest_qsort(L, []).

bestest_qsort([], Acc) -> Acc;
bestest_qsort([Pivot|Rest], Acc) ->
    bestest_partition(Pivot, Rest, {[], [Pivot], []}, Acc).

bestest_partition(_, [], {Smaller, Equal, Larger}, Acc) ->
    bestest_qsort(Smaller, Equal ++ bestest_qsort(Larger, Acc));
bestest_partition(Pivot, [H|T], {Smaller, Equal, Larger}, Acc) ->
    if H < Pivot ->
           bestest_partition(Pivot, T, {[H|Smaller], Equal, Larger}, Acc);
       H > Pivot ->
           bestest_partition(Pivot, T, {Smaller, Equal, [H|Larger]}, Acc);
       H == Pivot ->
           bestest_partition(Pivot, T, {Smaller, [H|Equal], Larger}, Acc)
    end.