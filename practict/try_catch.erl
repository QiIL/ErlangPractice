-module(try_catch).
-compile(export_all).

init() ->
    ets:new(test, [set, named_table]),
    ets:insert(test, [{aaa, bbb, 1}, {aa, bb}]).

check() ->
    try ets:lookup_element(test, aa, 3) of
        _->
           io:format("lalalalalla") 
    catch
        _ ->
            io:format("jjjjjjjjjjjjjjjj")
    end.
