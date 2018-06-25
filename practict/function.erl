-module(function).
-compile([debug_info, export_all]).

%% 函数子句
greet(male, Name) ->
    io:format("Hello, Mr. ~s！", [Name]);
greet(female, Name) ->
    io:format("Hello, Miss ~s!", [Name]);
greet(_, Name) ->
    io:format("Hello, ~s!", [Name]).

%% io输出样式测试
io_format_test() ->
    io:format("~s~n", [<<"Hello">>]),
    io:format("~p~n", [<<"Hello">>]),
    io:format("~~~n"),
    io:format("~f~n", [4.0]),
    io:format("~30f~n", [4.0]).

%%% 模式匹配相关
%% 判断一样语句
same(X, X) ->
    true;
same(_, _) ->
    false.

%% 下面函数能够输出正确的时间格式
valid_time({Date = {Y, M, D}, Time = {H, Min, S}}) ->
    io:format("The Date tuple (~p) says today is: ~p/~p/~p, ~n", [Date, Y, M, D]),
    io:format("The Time tuple (~p) indicates: ~p:~p:~p. ~n", [Time, H, Min, S]);
valid_time(_) ->
    io:format("Please don't feeding me wrong Date data!~n").

%% 卫语句 guard语句
old_enough(X) when X >= 16 -> true;
old_enough(_) -> false.

right_age(X) when X >= 16, X =< 104 -> true;
right_age(_) -> false.

wrong_age(X) when X =< 16; X >= 104 -> true;
wrong_age(_) -> false.

%% andalso和orelse与“,”和“;”的对比
andalso_orelse_test(X, N) when X / 0 >= N; N >= 0 -> true;
andalso_orelse_test(_, _) -> false.
