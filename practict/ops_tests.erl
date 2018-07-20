-module(ops_tests).
-include_lib("eunit/include/eunit.hrl").

add_test() ->
    4 = ops:add(2, 2).

new_add_test() ->
    ?assertEqual(4, ops:add(2, 2)),
    ?assertEqual(3, ops:add(1, 2)),
    ?assert(is_number(ops:add(1, 2))),
    ?assertEqual(3, ops:add(2, 2)),
    ?assertError(badarith, 1/0).

add_test_() ->
    [
     test_them_types(),
     test_them_values(),
     ?_assertError(badarith, 1/0)
    ].

test_them_types() ->
    ?_assert(is_number(ops:add(1, 2))).

test_them_values() ->
    [?_assertEqual(4, ops:add(2, 2)),
    ?_assertEqual(3, ops:add(1, 2)),
    ?_assertEqual(3, ops:add(2, 2))].

double_register_test_() ->
    {setup,
     fun start/0, % 初始化函数
     fun stop/1, % 清理函数
     fun two_name_one_pid/1}. % 实例化器

start() ->
    {ok, Pid} = registry:start_link(),
    Pid.

stop(Pid) ->
    registry:stop(Pid).

two_name_one_pid(Pid) ->
    ok = registry:register(Pid, quite_a_unique_name, self()),
    Res = registry:register(Pid, my_other_name_is_more_creative, self()),
    [?_assertEqual({error, already_named}, Res)].
