-module(postman).
-compile(export_all).
-export([post_feature/1]).

%% 这个模块的作用：1.如果需要添加Socket就把Socket添加进去告诉进程，
%% 2.如果需要广播，就拿一堆socket然后逐个给发消息
post_feature(SocketList) ->
    receive
        {add, Socket} ->
            post_feature([Socket|SocketList]);
        {sub, Socket} ->
            post_feature(lists:delete(Socket, SocketList));
        {broadcast, Socket, Msg} ->
            boardcast(SocketList, Socket, Msg),
            post_feature(SocketList)
    end.

boardcast([], _, _) ->
    ok;
boardcast([H|T], Socket, Str) ->
    case H =:= Socket of
        true ->
            gen_tcp:send(H, term_to_binary(Str)),
            boardcast(T, Socket, Str);
        false ->
            gen_tcp:send(H, term_to_binary(Str)),
            boardcast(T, Socket, Str)
    end.
    