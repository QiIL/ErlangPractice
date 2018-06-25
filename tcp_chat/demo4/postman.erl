-module(postman).
-compile(export_all).
-export([post_feature/2]).

%% 这个模块的作用：1.如果需要添加Socket就把Socket添加进去告诉进程，
%% 2.如果需要广播，就拿一堆socket然后逐个给发消息
post_feature(SocketList, NickList) ->
    receive
        {broadcast, Socket, Msg} ->
            io:format("~p~n", [SocketList]),
            boardcast(SocketList, Socket, Msg),
            post_feature(SocketList, NickList);
        {online, Socket, Nick} ->
            case lists:member(Nick, NickList) of
                true -> 
                    io:format("~p~n", [NickList]),
                    gen_tcp:send(Socket, term_to_binary("the name: " ++ Nick ++ " has been use, goodbye!")),
                    gen_tcp:close(Socket),
                    post_feature(SocketList, NickList);
                false ->
                    boardcast(SocketList, Socket, string:concat(Nick, " is online!")),
                    post_feature([Socket|SocketList], [Nick| NickList])
            end;
        {quit, Socket, Nick} ->
            boardcast(SocketList, Socket, string:concat(Nick, " is quit~~")),
            post_feature(lists:delete(Socket, SocketList), lists:delete(Nick, NickList)),
            gen_tcp:close(Socket)
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
    