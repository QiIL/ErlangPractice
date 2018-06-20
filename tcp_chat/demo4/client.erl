-module(client).
-export([eval/1, connect/1, chat/2, say/2, quit/1]).

eval(Str) ->
    {ok, Socket} = gen_tcp:connect("localhost", 4000, [binary]),
    ok = gen_tcp:send(Socket, term_to_binary(Str)),
    receive
        {tcp, Socket, Bin} ->
            io:format("Client receive binary = ~p~n", [Bin]),
            Val = binary_to_term(Bin),
            io:format("Client result ~p~n", [Val]),
            gen_tcp:close(Socket)
    end.

%%% 连接到服务器，然后向服务器发消息直到想要退出为止。
connect(Nickname) ->
    {ok, Socket} = gen_tcp:connect("localhost", 4000, [binary]),
    Pid = spawn(?MODULE, chat, [Nickname, Socket]),
    gen_tcp:controlling_process(Socket, Pid),
    Pid.
    
%% 聊天界面，收到tcp消息就显示，若是其他命令则做其他事情
chat(Nickname, Socket) ->
    receive
        {tcp, Socket, Bin} ->
            Val = binary_to_term(Bin),
            io:format("~p~n", [Val]),
            chat(Nickname, Socket);
        {send, Msg} ->
            gen_tcp:send(Socket, term_to_binary(lists:concat([Nickname, ":", Msg]))),
            chat(Nickname, Socket);
        {quit} ->
            gen_tcp:close(Socket)
    end.

say(Pid, Str) ->
    Pid ! {send, Str},
    ok.

quit(Pid) ->
    Pid ! {quit}.
