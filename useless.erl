-module(useless).
-compile([debug_info, export_all]).
-export([add/2, hello/0, greet_and_add_tow/1]).

-define(HOUR, 3600).

add(A, B) -> A + B.

%% 显示欢迎语
%% io:format(/1是标准的文本输出)

hello() ->
    io:format("Hello, world!~n").

greet_and_add_tow(X) ->
    hello(), add(X, 2).

-ifdef(DEBUGMODE).
-define(DEBUG(S), io:format("dbg: " ++S)).
-else.
-define(DEBUG(S), ok).
-endif.

-ifdef(TEST).
my_test_func() ->
    io:format(?HOUR).
-endif.