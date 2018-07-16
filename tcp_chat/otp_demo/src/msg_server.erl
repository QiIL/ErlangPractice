-module(msg_server).
-export([
    new/1,
    init/1, handle_call/3, handle_cast/2,
    handle_info/2, terminate/2, code_change/3
]).
-behaviour(gen_server).
-include("../include/records.hrl").
-record(chat, {user, pass, socket}).

new(Socket) -> gen_server:start_link(?MODULE, [Socket], []).

init([Socket]) -> {ok, #chat{socket=Socket}}.

handle_call(Commend, _From, Chat) ->
    {reply, Commend, Chat}.

handle_cast(Msg, Chat) ->
    io:format("Unknow message ~p, ~n", [Msg]),
    {noreply, Chat}.

handle_info({tcp, _Socket, Bin}, Chat) ->
    Msg = binary_to_term(Bin),
    NewChat = handle_msg(Msg, Chat#chat.socket, Chat),
    {noreply, NewChat};
handle_info({tcp_closed, _Socket}, Chat) ->
    {stop, normal, Chat}.

%% 终结
terminate(normal, Chat) ->
    io:format("quit now~n"),
    deal_socket(minus, Chat#chat.socket),
    ets:insert(users, {Chat#chat.user, Chat#chat.pass, false}),
    group_offline(ets:match_object(groups, {'$1', '$2', Chat#chat.user, '$3'}), Chat#chat.socket),
    ok.

%% 热更新
code_change(_OldVsn, Socket, _Extra) ->
    {ok, Socket}.

%% 私有函数
handle_msg({login, Username, Pass}, Socket, Chat) ->
    case check_user(ets:lookup(users, Username), Pass, true) of
        {ok, _, _} ->
            ets:insert(users, {Username, Pass, true, Socket}),
            deal_socket(add, Socket),
            io:format("groups: ~p~n", [ets:match_object(groups, {{Username, '_'}, '$1', '$2'})]),
            group_online(ets:match_object(groups, {{Username, '_'}, '$1', '$2'}), Socket),
            send_msg(Socket, {login_success, Username});
        {ok, _, _, _} ->
            %% 先关掉原先的socket，然后再重新插入
            [{_, _, _, OriginSocket}] = ets:lookup(users, Username),
            deal_socket(replace, OriginSocket, Socket),
            io:format("groups: ~p~n", [ets:match_object(groups, {{Username, '_'}, '$1', '$2'})]),
            group_offline(ets:match_object(groups, {{Username, '_'}, '$1', '$2'}), OriginSocket),
            group_online(ets:match_object(groups, {{Username, '_'}, '$1', '$2'}), Socket),
            ets:insert(users, {Username, Pass, true, Socket}),
            send_msg(OriginSocket, {squit, "your account is logining in other place"}),
            send_msg(Socket, {login_success, Username});
        {err, Reason} ->
            io:format("handle_msg reason: ~p~n", [Reason]),
            send_msg(Socket, {err, Reason})
    end,
    Chat#chat{user=Username, pass=Pass};
handle_msg(showskl, _Socket, Chat) ->
    io:format("SocketLists is : ~p~n", [ets:lookup_element(socket_list, sockets, 2)]),
    Chat;
handle_msg(showets, _Socket, Chat) ->
    Ets3 = ets:match(users, {'$1', '$2', '$3'}),
    Ets4 = ets:match(users, {'$1', '$2', '$3', '$4'}),
    io:format("ets3: ~p~n", [Ets3]),
    io:format("ets4: ~p~n", [Ets4]),
    Chat;
handle_msg({change_pass, Username, OldPass, NewPass}, Socket, Chat) ->
    case check_user(ets:lookup(users, Username), OldPass, true) of
        {ok, _User, _Pass} ->
            io:format("new password = :~p~n", [NewPass]),
            ets:insert(users, {Username, NewPass, false}),
            mmnesia:change_pass(Username, NewPass),
            deal_socket(minus, Socket),
            send_msg(Socket, {change_pass_success, Username}),
            send_msg(Socket, "Pass change success!");
        {ok, _User, _Pass, _Socket} ->
            io:format("new password = :~p~n", [NewPass]),
            ets:insert(users, {Username, NewPass, false}),
            mmnesia:change_pass(Username, NewPass),
            deal_socket(minus, Socket),
            send_msg(Socket, {change_pass_success, Username}),
            send_msg(Socket, "Pass change success!");
        {err, Reason} ->
            io:format("handle_msg reason: ~p~n", [Reason]),
            send_msg(Socket, {err, Reason})
    end,
    Chat#chat{pass = NewPass};
handle_msg({kick, Username, Kuser}, _Socket, Chat) ->
    kick_user(ets:lookup(users, Username), ets:lookup(users, Kuser)),
    Chat;
handle_msg({talk, User, Msg}, Socket, Chat) ->
    mmnesia:save_rec(broadcast, User, all, Msg),
    broadcast(ets:lookup_element(socket_list, sockets, 2), Socket, User, Msg),
    Chat;
handle_msg({whisper, User, ToUser, Msg}, Socket, Chat) ->
    case ets:lookup(users, ToUser) of
        [] -> send_msg(Socket, "User: " ++ atom_to_list(ToUser) ++ " isn't exist");
        [{_, _, false}] -> send_msg(Socket, "User: " ++ atom_to_list(ToUser) ++ " not online");
        [{_, _, true, ToSocket}] -> 
            mmnesia:save_rec(whisper, User, ToUser, Msg),
            send_msg(ToSocket, {whisper, User, Msg})
    end,
    Chat;
handle_msg(check_online, Socket, Chat) ->
    send_msg(Socket, "the online number is: " ++ integer_to_list(check_online_num())),
    Chat;
handle_msg({new_group, Username, GroupName}, Socket, Chat) ->
    {ok, Id} = mmnesia:create_group(GroupName, Username),
    {ok, NewGroupPid} = groups:new(Id+1, GroupName, Username, Socket),
    ets:insert(groups, {{Username, Id+1}, GroupName, NewGroupPid}),
    send_msg(Socket, "new Group ok!"),
    Chat;
handle_msg({join_group, GroupId, Username}, Socket, Chat) ->
    [{_, Gname, GroupPid} | _] = ets:match_object(groups, {{'_', GroupId}, '$1', '$2'}),
    GroupPid !{join, Username, Socket},
    ets:insert(groups, {{Username, GroupId}, Gname, GroupPid}),
    Chat;
handle_msg({leave_group, GroupId, Username}, Socket, Chat) ->
    [{_, _, GroupPid}] = ets:match_object(groups, {{Username, GroupId}, '$1', '$2'}),
    GroupPid !{leave, Username, Socket},
    ets:delete(groups, {Username, GroupId}),
    Chat;
handle_msg({show_group, Username}, _Socket, Chat) ->
    UserGroups = ets:match_object(groups, {{Username, '_'}, '$1', '$2'}),
    io:format("~p~n", [UserGroups]),
    Chat;
handle_msg({group_speak, GroupId, Username, Msg}, Socket, Chat) ->
    [{_, _, GroupPid}] = ets:match_object(groups, {{Username, GroupId}, '$1', '$2'}),
    GroupPid !{speak, Username, Socket, Msg},
    mmnesia:save_rec(group_talk, Username, GroupId, Msg),
    Chat;
handle_msg({get_rec, Username}, Socket, Chat) ->
    Recs = get_chat_rec(Username),
    send_msg(Socket, {recs, Recs}),
    Chat;
handle_msg(Msg, _Socket, Chat) ->
    io:format("Unexpected tcp message ~p, ~n", [Msg]),
    Chat.

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

%% 广播
broadcast([], _, _, _) ->
    ok;
broadcast([H|T], Socket, User, Str) ->
    case H =:= Socket of
        true ->
            gen_tcp:send(H, term_to_binary({boardcast, "you", Str})),
            broadcast(T, Socket, User, Str);
        false ->
            gen_tcp:send(H, term_to_binary({boardcast, User, Str})),
            broadcast(T, Socket, User, Str)
    end.

%% 发送消息
send_msg(Socket, Msg) ->
    gen_tcp:send(Socket, term_to_binary(Msg)).

%% 加减Socket到SocketList
deal_socket(add, Socket) ->
    SocketList = ets:lookup_element(socket_list, sockets, 2),
    io:format("SocketList is: ~p~n", [SocketList]),
    ets:insert(socket_list, {sockets, [Socket | SocketList]});
deal_socket(minus, Socket) ->
    SocketList = ets:lookup_element(socket_list, sockets, 2),
    io:format("Old socketlist is: ~p~n", [SocketList]),
    ets:insert(socket_list, {sockets, lists:delete(Socket, SocketList)}),
    io:format("New socketlist is: ~p~n", [lists:delete(Socket, SocketList)]).
deal_socket(replace, OriginSocket, NewSocket) ->
    SocketList = ets:lookup_element(socket_list, sockets, 2),
    io:format("Old socketlist is: ~p~n", [SocketList]),
    ets:insert(socket_list, {sockets, [NewSocket | lists:delete(OriginSocket, SocketList)]}),
    io:format("New socketlist is: ~p~n", [[NewSocket | lists:delete(OriginSocket, SocketList)]]).

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

%% 查询在线数量
check_online_num() ->
    length(ets:lookup_element(socket_list, sockets, 2)).

group_online([], _) -> ok;
group_online([{_, _, GroupPid} | T], Socket) ->
    GroupPid ! {online, Socket},
    group_online(T, Socket).

group_offline([], _) -> ok;
group_offline([{_, _, GroupPid} | T], Socket) ->
    GroupPid ! {offline, Socket},
    group_offline(T, Socket).

get_chat_rec(User) ->
    R = #chat_record{user='$1', target='$2', _='_'},
    Guards = [{'orelse', {'==', '$1', User}, {'==', '$2', User}}],
    mmnesia:search(chat_record, R, Guards, ['$_']).
