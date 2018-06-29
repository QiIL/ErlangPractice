-module(server).
-compile(export_all).

start() ->
    {ok, Listen} = gen_tcp:listen(4000, [binary, {active, true}, {nodelay, true}]),
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
            Msg = binary_to_term(Bin),
            io:format("Server (unpacked) ~p~n", [Msg]),
            handle_msg(Msg, Socket),
            loop(Socket);
        {tcp_closed, Socket} ->
            Sl = lists:delete(Socket, ets:lookup_element(socket_list, sockets, 2)),
            ets:insert(socket_list, {sockets, Sl}),
            io:format("tcp server close!~n")
    end.

%% 处理返回消息函数
handle_msg(showets, _) ->
    Ets3 = ets:match(users, {'$1', '$2', '$3'}),
    Ets4 = ets:match(users, {'$1', '$2', '$3', '$4'}),
    io:format("ets3: ~p~n", [Ets3]),
    io:format("ets4: ~p~n", [Ets4]);
handle_msg({login, User, Pass}, Socket) ->
    case check_user(ets:lookup(users, User), Pass, true) of
        {ok, User, _Pass} ->
            ets:insert(users, {User, Pass, true, Socket}),
            send_msg(Socket, {login, User});
        {ok, User, _Pass, _Socket} ->
            %% 先关掉原先的socket，然后再重新插入
            [{_, _, _, OriginSocket}] = ets:lookup(users, User),
            gen_tcp:close(OriginSocket),
            ets:insert(users, {User, Pass, true, Socket}),
            send_msg(Socket, {login, User});
        {err, Reason} ->
            io:format("handle_msg reason: ~p~n", [Reason]),
            send_msg(Socket, {err, Reason})
    end;
handle_msg({change_pass, User, OldPass, NewPass}, Socket) ->
    case check_user(ets:lookup(users, User), OldPass, true) of
        {ok, _User, _Pass} ->
            ets:insert(users, {User, NewPass, false}),
            send_msg(Socket, {cp, User});
        {ok, _User, _Pass, _Socket} ->
            ets:insert(users, {User, NewPass, false}),
            send_msg(Socket, {cp, User});
        {err, Reason} ->
            io:format("handle_msg reason: ~p~n", [Reason]),
            send_msg(Socket, {err, Reason})
    end.

%% 发送消息
send_msg(Socket, Msg) ->
    io:format("Msg is : ~p~n", [Msg]),
    gen_tcp:send(Socket, term_to_binary(Msg)).

%% 创建用户
new_user(User, Pass) ->
    case ets:lookup(users, User) of
        [] -> ets:insert(users, {User, Pass, false}), {new, User};
        _ -> {err, "User exist"}
    end.

%% 检查用户存在性
%% 不检查密码
check_user([{User, Pass, _}], _, false) ->
    {ok, User, Pass};
%% 检查密码和存在性
check_user([{User, Pass, _}], Fpass, true) when Pass =:= Fpass ->
    {ok, User, Pass};
check_user([{_, _, _}], _, true) ->
    {err, "username/password is wrong"};
check_user([{User, Pass, _, Socket}], Fpass, true)  when Pass =:= Fpass ->
    {ok, User, Pass, Socket};
check_user([{_, _, _, _}], _, true) ->
    {err, "username/password is wrong"};
check_user([], _, _) ->
    {err, "account isn't exist"}.

%%  获取用户socket
get_socket(User) ->
    ets:lookup_element(users, User, 4).

% %% 修改密码
% change_pass(User, OldPass, NewPass) ->
%     case check_user(User) of
%         {ok, User, Pass} -> 
%             case OldPass =:= Pass of
%                 true -> ets:insert(users, {User, NewPass}), {ok, "change password success"};
%                 false -> {err, "Password is wrong!"}
%             end;
%         {Error, Reason} -> {Error, Reason}
%     end.

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
