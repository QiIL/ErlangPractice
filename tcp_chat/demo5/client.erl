-module(client).
-compile(export_all).

%% 注册
register_and_login(User, Pass) ->
    %% tcp连接
    {ok, Socket} = gen_tcp:connect("localhost", 4000, [binary]),
    gen_tcp:send(Socket, term_to_binary({login, User, Pass})),
    receive
        {tcp, _Socket, Bin} ->
            deel(binary_to_term(Bin)),
            spawn(?MODULE, chat, [Socket, User])
    end.

% %% 接收消息进程
% chat(Socket) ->
%     receive
%         {tcp, _Socket, Bin} ->
%             case binary_to_term(Bin) of
%                 {login, User} ->
%                     io:format("登陆成功！"),
%                     chat(Socket, User);
%                 _ ->
%                     io:format("why I am here?")
%             end;
%         {login, User, Pass} ->
%             io:format("login now~n"),
%             gen_tcp:send(Socket, term_to_binary({login, User, Pass})),
%             chat(Socket);
%         _ ->
%             io:format("请先登陆！~n"),
%             chat(Socket)
%     end.

%% 接收消息
chat(Socket, User) ->
    receive
        {tcp, _Socket, Bin} ->
            io:format("YES, I should be here"),
            deel(binary_to_term(Bin)),
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
            gen_tcp:send(Socket, term_to_binary({showets})),
            chat(Socket, User);
        quit ->
            io:format("quit now"),
            Val = term_to_binary({quit, User}),
            gen_tcp:send(Socket, Val),
            gen_tcp:close(Socket),
            io:format("Goodbye my friend!")
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
deel({boardcast, Msg}) ->
    io:format("[群聊]: ~p~n", [Msg]);
deel({secrect, FromUser, Msg}) ->
    io:format("[悄悄话-~p]: ~p~n", [FromUser, Msg]);
deel({new, _User}) ->
    io:format("注册成功！~n");
deel({err, Reason}) ->
    io:format("~p", [Reason]);
deel({login, _User}) ->
    io:format("登陆成功");
deel(Others) ->
    io:format("other: ~p~n", [Others]).
