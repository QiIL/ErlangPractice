-module(client).
-compile(export_all).

%% 注册
register_and_login(User, Pass) ->
    %% tcp连接
    {ok, Socket} = gen_tcp:connect("localhost", 4000, [binary, {nodelay, true}]),
    gen_tcp:send(Socket, term_to_binary({login, User, Pass})),
    receive
        {tcp, _Socket, Bin} ->
            deal(binary_to_term(Bin), Socket),
            spawn(?MODULE, chat, [Socket, User])
    end.

%% 接收消息
chat(Socket, User) ->
    receive
        {tcp, _Socket, Bin} ->
            io:format("YES, I should be here"),
            deal(binary_to_term(Bin), Socket),
            chat(Socket, User);
        {talk, Msg} ->
            io:format("talk now~n"),
            Val = term_to_binary({talk, User, Msg}),
            gen_tcp:send(Socket, Val),
            chat(Socket, User);
        {secrect, ToUser, Msg} ->
            io:format("secrect now"),
            Val = term_to_binary({secrect, User, ToUser, Msg}),
            gen_tcp:send(Socket, Val),
            chat(Socket, User);
        {change_pass, OldPass, NewPass} ->
            io:format("change_pass now"),
            Val = term_to_binary({change_pass, User, OldPass, NewPass}),
            gen_tcp:send(Socket, Val),
            chat(Socket, User);
        showets ->
            io:format("show ets now ~n"),
            gen_tcp:send(Socket, term_to_binary(showets)),
            chat(Socket, User);
        quit ->
            io:format("quit now"),
            Val = term_to_binary({quit, User}),
            gen_tcp:send(Socket, Val),
            gen_tcp:close(Socket),
            io:format("Goodbye my friend!");
        {tcp_closed, _Socket} ->
            io:format("tcp client close!~n")
    end.

%% 登陆
login(Pid, User, Pass) ->
    Pid ! {login, User, Pass}.
    
%% 查看数据库
showets(Pid) ->
    Pid ! showets.

%% 说话
say(Pid, Msg) ->
    Pid ! {talk, Msg}.
say(Pid, User, Str) ->
    Pid ! {secrect, User, Str}.

%% 改密码
change_pass(Pid, OldPass, NewPass) ->
    Pid ! {change_pass, OldPass, NewPass}.

%% 退出
quit(Pid) ->
    Pid ! quit.

%% 处理TCP回复消息
deal({boardcast, Msg}, _) ->
    io:format("[群聊]: ~p~n", [Msg]);
deal({secrect, FromUser, Msg}, _) ->
    io:format("[悄悄话-~p]: ~p~n", [FromUser, Msg]);
deal({new, _User}, _) ->
    io:format("注册成功！~n");
deal({err, Reason}, Socket) ->
    gen_tcp:close(Socket),
    io:format("~p~n", [Reason]);
deal({login, _User}, _) ->
    io:format("登陆成功");
deal({cp, _User}, Socket) ->
    io:format("you success change the pass"),
    gen_tcp:close(Socket),
    io:format("密码修改成功！");
deal(Others, _) ->
    io:format("other: ~p~n", [Others]).
