-module(client).
-compile(export_all).

init() ->
    ets:new(client_list, [order_set, named_table]).

%% 注册
register(Username, Pass) ->
    jenkins:new_user(Username, Pass).

%% 登陆
login(Username, Pass) ->
    case jenkins:check_user(Username, Pass) of
        true ->
            %% tcp连接
            {ok, Socket} = gen_tcp:connect("localhost", 4000, [binary]),
            Pid = spawn(?MODULE, chat, [Socket]),
            gen_tcp:controlling_process(Socket, Pid),
            ets:new(own, [set, named_table]),
            chat(Pid, Socket, Username);
        none ->
            io:format("this account isn't exist, please regist first");
        wrong_pass ->
            io:format("Wrong Password or Username, please check your input")
    end.

%% 改密码
change_pass(Username, OldPass, NewPass) ->
    jenkins:change_pass(Username, OldPass, NewPass).
    
%% 接收消息
chat(Pid, Socket, Username) ->
    receive
        {tcp, _Socket, Bin} ->
            Val = binary_to_term(Bin),
            io:format("~p~n", [Val]),
            chat(Pid, Socket, Username)
    end.

%% 说话
say(Str) ->
    jenkins:speak_to_all(Str).
say(Obj, Str) ->
    jenkins:speak_to_one(Obj, Str).

%% 退出
quit() ->
    jenkins:quit().


