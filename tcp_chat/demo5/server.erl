-module(server).
-compile(export_all).

start() ->
    {ok, Listen} = gen_tcp:listen(4000, [binary, {active, true}]),
    ets:new(users, [set, public, named_table]),
    ets:insert(users, {admin, admin, false}),
    ets:insert(users, {qill, 11, false}),
    ets:new(socket_list, [set, public, named_table]),
    ets:insert(socket_list, {sockets, []}),
    pre_loop(Listen).

pre_loop(Listen) ->
    {ok, Socket} = gen_tcp:accept(Listen),
    SocketList = ets:lookup_element(socket_list, sockets, 2),
    io:format("SocketList is: ~p~n", [SocketList]),
    ets:insert(socket_list, {sockets, [Socket | SocketList]}),
    spawn(fun() -> pre_loop(Listen) end),
    loop(Socket).

loop(Socket) ->
    receive
        {tcp, _Socket, Bin} ->
            Str = binary_to_term(Bin),
            io:format("Server (unpacked) ~p~n", [Str]),
            case Str of
                {change_pass, User, OldPass, NewPass} ->
                    io:format("change pass now~n"),
                    gen_tcp:send(Socket, term_to_binary(change_pass(User, OldPass, NewPass)));
                {login, User, Pass} ->
                    io:format("login now~n"),
                    case check_user(User) of
                        {ok, _, _} ->
                            ets:insert(users, {User, Pass, true, Socket}),
                            gen_tcp:send(Socket, term_to_binary({login, User}));
                        {err, _} ->
                            gen_tcp:send(Socket, term_to_binary({err, "用户名/密码错误！"})),
                            gen_tcp:close(Socket)
                    end;
                {talk, _User, Msg} ->
                    io:format("speaking~n"),
                    boardcast(ets:lookup_element(socket_list, sockets, 2), Socket, Msg);
                {secret, FromUser, ToUser, Msg} ->
                    io:format("secret~n"),
                    Tsocket = get_socket(ToUser),
                    gen_tcp:send(Tsocket, term_to_binary({secret, FromUser, Msg}));
                {quit, User} ->
                    io:format("quit~n"),
                    boardcast(ets:lookup_element(socket_list, sockets, 2), Socket, User ++ "下线了~~");
                {showets} ->
                    Ets3 = ets:match(users, {'$1', '$2', '$3'}),
                    Ets4 = ets:match(users, {'$1', '$2', '$3', '$4'}),
                    io:format("ets3: ~p~n", [Ets3]),
                    io:format("ets4: ~p~n", [Ets4]);
                Other ->
                    io:format("server get ~p", [Other])
            end,
            loop(Socket);
        {tcp_closed, Socket} ->
            Sl = lists:delete(Socket, ets:lookup_element(socket_list, sockets, 2)),
            ets:insert(socket_list, {sockets, Sl}),
            io:format("tcp server close!~n")
    end.

%% 创建用户
new_user(User, Pass) ->
    case ets:lookup(users, User) of
        [] -> ets:insert(users, {User, Pass, false}), {new, User};
        _ -> {err, "User exist"}
    end.

%% 检查用户是否存在
check_user(User) ->
    case ets:lookup(users, User) of
        [] -> {no_user, "this account isn't exist, please regist first"};
        [{User, Pass, _}] -> {ok, User, Pass};
        [{User, Pass, _, Socket}] -> {ok, User, Pass, Socket}
    end.

%%  获取用户socket
get_socket(User) ->
    ets:lookup_element(users, User, 4).

%% 修改密码
change_pass(User, OldPass, NewPass) ->
    case check_user(User) of
        {ok, User, Pass} -> 
            case OldPass =:= Pass of
                true -> ets:insert(users, {User, NewPass}), {ok, "change password success"};
                false -> {err, "Password is wrong!"}
            end;
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
