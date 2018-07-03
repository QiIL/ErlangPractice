-module(groups).
-compile(export_all).

%% 根据名字生成进程进入循环
new(GroupId, GroupName, Owner, OwnerSocket) ->
    spawn(?MODULE, loop, [GroupId, GroupName, Owner, [Owner], OwnerSocket]).

%% 循环
loop(GroupId, GroupName, Owner, UserList, SocketList) ->
    receive
        {add_member, User, Socket} ->
            case lists:member(User, UserList) of
                true ->
                    gen_tcp:send(Socket, term_to_binary("you are in this Group: ~p already!~n", [GroupName]));
                false ->
                    io:format("~p join group: ~p success!~n", [User, GroupName]),
                    io:format("Group user list: ~p~n", [[User | UserList]]),
                    io:format("Group socket list: ~p~n", [[Socket | SocketList]])
            end,
            loop(GroupId, GroupName, Owner, [User | UserList], [Socket | SocketList]);
        {sub_member, User} ->
            loop(GroupId, GroupName, Owner, lists:delete(User, UserList), SocketList);
        {sub_member, User, Socket} ->
            case lists:member(User, UserList) of
                false ->
                    gen_tcp:send(Socket, term_to_binary("you are not in this Group: ~p~n!", [GroupName]));
                true ->
                    io:format("~p leave group: ~p success!~n", [User, GroupName]),
                    io:format("Group user list: ~p~n", [lists:delete(User, UserList)]),
                    io:format("Group socket list: ~p~n", [lists:delete(Socket, SocketList)])
            end,
            loop(GroupId, GroupName, Owner, lists:delete(User, UserList), lists:delete(Socket, SocketList));
        {speak, User, Socket, Msg} ->
            case lists:member(User, UserList) of
                true ->
                    io:format("~p group talk: ~p~n", [User, GroupName]),
                    io:format("Group user list: ~p~n", [UserList]),
                    io:format("Group socket list: ~p~n", [SocketList]),
                    boardcast(SocketList, Socket, User, Msg, GroupName);
                false ->
                    gen_tcp:send(Socket, term_to_binary("you are not in this group"))
            end,
            loop(GroupId, GroupName, Owner, UserList, SocketList);
        {online, Socket} ->
            io:format("online ~p~n", [GroupName]),
            io:format("Socketlist in group is: ~p~n", [[Socket| SocketList]]),
            loop(GroupId, GroupName, Owner, UserList, [Socket| SocketList]);
        {offline, Socket} ->
            io:format("offline ~p~n", [GroupName]),
            io:format("Socketlist in group is: ~p~n", [lists:delete(Socket, SocketList)]),
            loop(GroupId, GroupName, Owner, UserList, lists:delete(Socket, SocketList))
    end.

%% 发送消息
boardcast([], _, _, _, _) ->
    ok;
boardcast([H|T], Socket, User, Str, GroupName) ->
    case H =:= Socket of
        true ->
            gen_tcp:send(H, term_to_binary({boardcast, GroupName, "you", Str})),
            boardcast(T, Socket, User, Str, GroupName);
        false ->
            gen_tcp:send(H, term_to_binary({boardcast, GroupName, User, Str})),
            boardcast(T, Socket, User, Str, GroupName)
    end.

