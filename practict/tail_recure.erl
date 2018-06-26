-module(tail_recure).
-compile(export_all).

fun(N, Sum) -> 
    if N =:= 0 ->
        Sum
    true ->
        fun(N - 1, Sum + N)
    end