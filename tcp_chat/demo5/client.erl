-module(client).
-compile(export_all).

%% 注册
register_and_login(User, Pass) ->
    %% 聊天进程
    {{_, _, _}, {_, Min, _}} = calendar:now_to_local_time(os:timestamp()),
    spawn(?MODULE, chat, [User, Pass, Min, 0]).

%% 接收消息
chat(User, Pass, Min, ChatNum) ->
    {ok, Socket} = gen_tcp:connect("192.168.15.98", 4000, [binary, {nodelay, true}]),
    gen_tcp:send(Socket, term_to_binary({login, User, Pass})),
    loop(Socket, User, Min, ChatNum).

loop(Socket, User, Min, ChatNum) ->
    receive
        {tcp, _Socket, Bin} ->
            deal(binary_to_term(Bin), Socket),
            loop(Socket, User, Min, ChatNum);
        online_num ->
            gen_tcp:send(Socket, term_to_binary(check_online)),
            loop(Socket, User, Min, ChatNum);
        showets ->
            io:format("show ets now ~n"),
            gen_tcp:send(Socket, term_to_binary(showets)),
            loop(Socket, User, Min, ChatNum);
        {change_pass, OldPass, NewPass} ->
            io:format("change_pass now~n"),
            Val = term_to_binary({change_pass, User, OldPass, NewPass}),
            gen_tcp:send(Socket, Val),
            loop(Socket, User, Min, ChatNum);
        {group, GroupName} ->
            gen_tcp:send(Socket, term_to_binary({group, User, GroupName})),
            loop(Socket, User, Min, ChatNum);
        show_group -> 
            io:format("show group now ~n"),
            gen_tcp:send(Socket, term_to_binary(show_group)),
            loop(Socket, User, Min, ChatNum);
        {talk, GroupId, Msg} ->
            io:format("talk now~n"),
            {{_, _, _}, {_, NewMins, _}} = calendar:now_to_local_time(os:timestamp()),
            case {Min =:= NewMins, ChatNum < 50} of
                {true, true} -> 
                    Val = term_to_binary({talk, GroupId, User, Msg}),
                    gen_tcp:send(Socket, Val),
                    loop(Socket, User, NewMins, ChatNum+1);
                {true, false} ->
                    io:format("you talk too fast, emmmmmm you should take a coffee and have a rest~n"),
                    loop(Socket, User, NewMins, ChatNum);
                {false, _} ->
                    Val = term_to_binary({talk, GroupId, User, Msg}),
                    gen_tcp:send(Socket, Val),
                    loop(Socket, User, NewMins, 0)
            end;
        {secrect, ToUser, Msg} ->
            io:format("secrect now~n"),
            Val = term_to_binary({secrect, User, ToUser, Msg}),
            gen_tcp:send(Socket, Val),
            loop(Socket, User, Min, ChatNum);
        {kick, Kuser} ->
            io:format("kicking ~p~n", [Kuser]),
            gen_tcp:send(Socket, term_to_binary({kick, User, Kuser})),
            loop(Socket, User, Min, ChatNum);
        quit ->
            io:format("quit now~n"),
            Val = term_to_binary({quit, User}),
            gen_tcp:send(Socket, Val),
            io:format("Goodbye my friend!~n");
        squit ->
            io:format("Squit, force goodbye my friend~n")
    end.

%% 查看数据库
showets(Pid) ->
    Pid ! showets.

%% 在线人数
online_num(Pid) ->
    Pid ! online_num.

%% 登陆
login(Pid, User, Pass) ->
    Pid ! {login, User, Pass}.

%% 改密码
change_pass(Pid, OldPass, NewPass) ->
    Pid ! {change_pass, OldPass, NewPass}.

%% 新建群组
new_group(Pid, GroupName) ->
    Pid ! {group, GroupName}.

%% 查看群组
show_group(Pid) ->
    Pid ! show_group.

%% 说话
say(Pid, Msg) ->
    Pid ! {talk, 1, Msg}.
say(Pid, GroupId, Msg) ->
    Pid ! {talk, GroupId, Msg}.

%% 私聊
whisper(Pid, User, Str) ->
    Pid ! {secrect, User, Str}.

%% 踢人
kick(Pid, User) ->
    Pid ! {kick, User}.

%% 退出
quit(Pid) ->
    Pid ! quit.

%% 处理TCP回复消息
deal({login, _User}, _) ->
    io:format("登陆成功~n");
deal({cp, _User}, _Socket) ->
    io:format("~p~n", [self()]),
    io:format("you success change the pass~n"),
    io:format("密码修改成功！~n"),
    self() ! quit;
deal({group, GroupId, GroupName, UserList}, _) ->
    io:format("New group [~p] success! the groupId = ~p~n", [GroupName, GroupId]),
    io:format("New group userlist is: ~p~n", [UserList]);
deal({boardcast, User, Msg}, _) ->
    io:format("~p: ~p~n", [User, Msg]);
deal({secrect, FromUser, Msg}, _) ->
    io:format("Whisper(~p): ~p~n", [FromUser, Msg]);
deal({err, Reason}, Socket) ->
    gen_tcp:close(Socket),
    io:format("~p~n", [Reason]),
    self() ! quit;
deal({squit, Reason}, _Socket) ->
    io:format("quit because: ~p~n", [Reason]),
    self() ! squit;
deal(Others, _) ->
    io:format("other: ~p~n", [Others]).
