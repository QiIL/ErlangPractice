-module(client).
-compile(export_all).

%% 注册
register(User, Pass) ->
    jenkins:new_user(User, Pass).

%% 登陆
login(User, Pass) ->
    case jenkins:check_user(User, Pass) of
        {pass} ->
            %% tcp连接
            {ok, Socket} = gen_tcp:connect("localhost", 4000, [binary]),
            Pid = spawn(?MODULE, chat, [Socket, User]),
            online(Pid),
            Pid;
        {_, Reason} ->
            io:format("~p~n", [Reason]),
            io:format("this account isn't exist, please regist first~n");
        wrong_pass ->
            io:format("Wrong Password or Username, please check your input~n");
        _ ->
            io:format("jenkins is angry and choose not to show you anythings~n")
    end.

%% 接收消息
chat(Socket, User) ->
    receive
        {tcp, _Socket, Bin} ->
            Val = binary_to_term(Bin),
            case Val of
                {broadcast, Msg} ->
                    io:format("[群聊]: ~p~n", [Msg]);
                {secrect, FromUser, Msg} ->
                    io:format("[悄悄话-~p]: ~p~n", [FromUser, Msg])
            end,
            chat(Socket, User);
        {talk, Msg} ->
            Val = term_to_binary({talk, User, Msg}),
            gen_tcp:send(Socket, Val),
            chat(Socket, User);
        {secrect, ToUser, Msg} ->
            Val = term_to_binary({secrect, User, ToUser, Msg}),
            gen_tcp:send(Socket, Val),
            chat(Socket, User);
        {online} ->
            Val = term_to_binary({online, User}),
            gen_tcp:send(Socket, Val),
            chat(Socket, User);
        {change_pass, OldPass, NewPass} ->
            Val = term_to_binary({change_pass, User, OldPass, NewPass}),
            gen_tcp:send(Socket, Val),
            chat(Socket, User);
        {quit} ->
            Val = term_to_binary({quit, User}),
            gen_tcp:send(Socket, Val),
            gen_tcp:close(Socket)
    end.

%% 上线
online(Pid) ->
    Pid ! {online}.

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
    Pid ! {quit}.


