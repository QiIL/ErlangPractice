-module(discrep3).
-export([run/0]).

run() ->
    Tup = money(5, you),
    some_op(item(count, Tup), item(acount, Tup)).

money(Num, Name) -> {give, Num, Name}.

item(count, {give, X, _}) -> X;
item(acount, {give, _, X}) -> X.

some_op(A, B) -> A + B.