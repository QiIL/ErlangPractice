-module(server).
-compile(export_all).

start() ->
    {ok, Listen} = gen_tcp:listen(4000, [binary, {active, true}]),
    ets:new(users, [set, named_table]),
    ets:new(socket_list, [set, named_table]),
    ets:insert(socket_list, {sockets, []}),
    pre_loop(Listen).

pre_loop(Listen) ->
    {ok, Socket} = gen_tcp:accept(Listen),
    SocketList = ets:lookup_element(socket_list, sockets, 2),
    ets:insert(socket_list, {sockets, [Socket | SocketList]}),
    spawn(fun() -> pre_loop(Listen) end),
    loop(Socket).

loop(Socket) ->
    receive
        {tcp, Socket, Bin} ->
            Str = binary_to_term(Bin),
            io:format("Server (unpacked) ~p~n", [Str]),
            case Str of
                {new, User, Pass} ->
                    Reply = new_user(User, Pass, Socket),

                    gen_tcp
                    loop(Socket);
                {change_pass, User, OldPass, NewPass} ->
                    
                {talk, User, Msg} ->
                    
                {secret, FromUser, ToUser, Msg} ->
                    
                {online, User} ->
                    
                {quit, User} ->
            end,
            loop(Jenkins, Socket);
        {tcp_closed, _Socket} ->
            io:format("tcp server close!~n")
    end.

%% 创建用户
new_user(User, Pass, Socket) ->
    case ets:lookup(users, User) of
        [] -> ets:insert(users, {User, Pass, Socket}), ok;
        _ -> exist
    end.

%% 检查用户是否存在
check_user(User, Pass) ->
    case ets:lookup(User) of
        [] -> {no_user, "this account isn't exist, please regist first"};
        [{_, _}] ->
            case CheckPass =:= Pass of
                true -> {pass};
                false -> {wrong_pass, "Wrong Password or Username, please check your input"}
            end;
        _ ->
            {unkonwn, "server is angry and choose not to show you anythings"}
    end.

%% 修改密码
change_pass(User, OldPass, NewPass) ->
    case check_user(User, OldPass) of
        {pass} -> ets:insert(users, {User, NewPass});
        {Error, Reason} -> {Error, Reason}
    end.

%% 广播
boardcast([], _, _) ->
        ok;
boardcast([H|T], Socket, Str) ->
    case H =:= Socket of
        true ->
            gen_tcp:send(H, term_to_binary({boardcast, Str})),
            boardcast(T, Socket, Str);
        false ->
            gen_tcp:send(H, term_to_binary({boardcast, Str})),
            boardcast(T, Socket, Str)
    end.

secret(FromUser, ToUser, Msg) ->
    [{_, _, FSocket}] = ets:lookup(FromUser),
    case ets:lookup(ToUser) of
        [] -> gen_tcp:send(FSocket, term_to_binary("No this User"));
        [{ToUser, _, Socket}] ->
            gen_tcp:send(Socket, term_to_binary({secret, FromUser, Msg}))
    end.
