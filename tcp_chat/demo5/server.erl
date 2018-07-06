-module(server).
-compile(export_all).

start() ->
    {ok, Listen} = gen_tcp:listen(4000, [binary, {active, true}, {nodelay, true}]),
    %% 用户表
    ets:new(users, [set, public, named_table]),
    ets:insert(users, [
        {admin, admin, false},
        {qill, 11, false},
        {user1, 11, false},
        {user2, 11, false},
        {user3, 11, false},
        {user4, 11, false}
    ]),
    %% 群组表
    ets:new(groups, [ordered_set, public, named_table]),
    NormalPid = groups:new(1, normal_people, qill, []),
    ets:insert(groups, [
        {1, normal_people, qill, NormalPid}
    ]),
    %% socket表
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
handle_msg(testalive, Socket) ->
    send_msg(Socket, "I am alive you stupid!!!");
handle_msg(showets, _) ->
    Ets3 = ets:match(users, {'$1', '$2', '$3'}),
    Ets4 = ets:match(users, {'$1', '$2', '$3', '$4'}),
    io:format("ets3: ~p~n", [Ets3]),
    io:format("ets4: ~p~n", [Ets4]);
%% 检查登陆人数
handle_msg(check_online, Socket) ->
    send_msg(Socket, "The number of online users is: " ++ integer_to_list(check_online_num()));
%% 登陆
handle_msg({login, User, Pass}, Socket) ->
    case check_user(ets:lookup(users, User), Pass, true) of
        {ok, User, _Pass} ->
            ets:insert(users, {User, Pass, true, Socket}),
            send_msg(Socket, {login, User}),
            add_group(ets:match_object(groups, {'$1', '$2', User, '$3'}), Socket);
        {ok, User, _Pass, _Socket} ->
            %% 先关掉原先的socket，然后再重新插入
            [{_, _, _, OriginSocket}] = ets:lookup(users, User),
            send_msg(OriginSocket, {squit, "your account was login in other place"}),
            ets:insert(socket_list, {sockets, lists:delete(Socket, ets:lookup_element(socket_list, sockets, 2))}),
            clean_group(ets:match_object(groups, {'$1', '$2', User, '$3'}), Socket),
            gen_tcp:close(OriginSocket),
            ets:insert(users, {User, Pass, true, Socket}),
            add_group(ets:match_object(groups, {'$1', '$2', User, '$3'}), Socket),
            send_msg(Socket, {login, User});
        {err, Reason} ->
            io:format("handle_msg reason: ~p~n", [Reason]),
            send_msg(Socket, {err, Reason})
    end;
%% 改密码
handle_msg({change_pass, User, OldPass, NewPass}, Socket) ->
    case check_user(ets:lookup(users, User), OldPass, true) of
        {ok, _User, _Pass} ->
            ets:insert(users, {User, NewPass, true, Socket}),
            send_msg(Socket, {cp, User}),
            send_msg(Socket, "Pass change success!");
        {ok, _User, _Pass, _Socket} ->
            ets:insert(users, {User, NewPass, true, Socket}),
            send_msg(Socket, {cp, User}),
            send_msg(Socket, "Pass change success!");
        {err, Reason} ->
            io:format("handle_msg reason: ~p~n", [Reason]),
            send_msg(Socket, {err, Reason})
    end;
%% 群组操作
handle_msg({new_group, User, GroupName}, Socket) ->
    Id = ets:last(groups),
    NewGroupPid = groups:new(Id+1, GroupName, User, Socket),
    ets:insert(groups, {Id+1, GroupName, User, NewGroupPid}),
    send_msg(Socket, "new Group ok!");
handle_msg({show_group, User}, _Socket) ->
    UserGroups = ets:match_object(groups, {'$1', '$2', User, '$3'}),
    io:format("~p~n", [UserGroups]);
handle_msg({group_talk, GroupId, User, Msg}, Socket) ->
    [{_, _, _, GroupPid}] = ets:match_object(groups, {GroupId, '$1', '$2', '$3'}),
    GroupPid ! {speak, User, Socket, Msg};
handle_msg({group_join, GroupId, User}, Socket) ->
    [{_, _, _, GroupPid}] = ets:match_object(groups, {GroupId, '$1', '$2', '$3'}),
    GroupPid ! {add_member, User, Socket};
handle_msg({group_quit, GroupId, User}, Socket) ->
    [{_, _, _, GroupPid}] = ets:match_object(groups, {GroupId, '$1', '$2', '$3'}),
    GroupPid ! {sub_member, User, Socket};
%% 聊天
handle_msg({talk, User, Msg}, Socket) ->
    boardcast(ets:lookup_element(socket_list, sockets, 2), Socket, User, Msg);
%% 私聊
handle_msg({secrect, User, ToUser, Msg}, Socket) ->
    case ets:lookup(users, ToUser) of
        [] -> send_msg(Socket, "User: " ++ atom_to_list(ToUser) ++ " isn't exist");
        [{_, _, false}] -> send_msg(Socket, "User: " ++ atom_to_list(ToUser) ++ " not online");
        [{_, _, true, ToSocket}] -> send_msg(ToSocket, {secrect, User, Msg})
    end;
%% 踢人
handle_msg({kick, User, Kuser}, _Socket) ->
        kick_user(ets:lookup(users, User), ets:lookup(users, Kuser));
%% 退出
handle_msg({quit, User}, Socket) ->
    [{_, Pass, _, _}] = ets:lookup(users, User),
    clean_group(ets:match_object(groups, {'$1', '$2', User, '$3'}), Socket),
    ets:insert(socket_list, {sockets, lists:delete(Socket, ets:lookup_element(socket_list, sockets, 2))}),
    ets:insert(users, {User, Pass, false}),
    io:format("tcp closed by quit order~n"),
    gen_tcp:close(Socket).

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

%% 查询在线数量
check_online_num() ->
    length(ets:lookup_element(socket_list, sockets, 2)).

%% 新建群组
new_group(GroupName, User) ->
    NewestId = ets:first(groups),
    ets:insert(groups, {NewestId+1, GroupName, [User]}),
    {NewestId+1, GroupName, [User]}.

%%  获取用户socket
get_socket(User) ->
    ets:lookup_element(users, User, 4).

%% 广播
boardcast([], _, _, _) ->
    ok;
boardcast([H|T], Socket, User, Str) ->
    case H =:= Socket of
        true ->
            gen_tcp:send(H, term_to_binary({boardcast, "you", Str})),
            boardcast(T, Socket, User, Str);
        false ->
            gen_tcp:send(H, term_to_binary({boardcast, User, Str})),
            boardcast(T, Socket, User, Str)
    end.

%% 踢人
kick_user([{admin, _, true, AdminSocket}], []) ->
    send_msg(AdminSocket, "the user isn't exist");
kick_user([{admin, _, true, AdminSocket}], [{Kuser, _, false}]) ->
    send_msg(AdminSocket, atom_to_list(Kuser) ++ " isn't online");
kick_user([{admin, _, true, AdminSocket}], [{Kuser, Kpass, true, KuserSocket}]) ->    
    send_msg(KuserSocket, {squit, "your was kick by manager"}),
    ets:insert(socket_list, {sockets, lists:delete(KuserSocket, ets:lookup_element(socket_list, sockets, 2))}),
    gen_tcp:close(KuserSocket),
    send_msg(AdminSocket, "Kick " ++ atom_to_list(Kuser) ++ "'s ass success!"),
    ets:insert(users, {Kuser, Kpass, false});
kick_user([{_OtherUsers, _, _, Socket}], _) ->
    send_msg(Socket, "you are not the manager user").

add_group([], _) -> ok;
add_group([{_, _, _, GroupPid} | T], Socket) ->
    GroupPid ! {online, Socket},
    add_group(T, Socket).

clean_group([], _) -> ok;
clean_group([{_, _, _, GroupPid} | T], Socket) ->
    GroupPid ! {offline, Socket},
    clean_group(T, Socket).