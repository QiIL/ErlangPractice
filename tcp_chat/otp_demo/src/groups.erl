%%% 用进程管理群组的模块，事实是可以不用进程去做这个事
%%% 适用到地（4）需求
-module(groups).
-export([
    new/3, new/4, join/3,
    leave/3, speak/4,
    online/2, offline/2
]).
-export([
    init/1, handle_call/3, handle_cast/2,
    handle_info/2, terminate/2, code_change/3
]).

-behaviour(gen_server).
-record(group, {id, name, users, sockets}).

new(GroupId, GroupName, Members) ->
    gen_server:start_link(?MODULE, [GroupId, GroupName, Members, []], []).

new(GroupId, GroupName, Members, SocketLists) ->
    gen_server:start_link(?MODULE, [GroupId, GroupName, Members, SocketLists], []).

join(Pid, User, Socket) -> gen_server:cast(Pid, {join, User, Socket}).
leave(Pid, User, Socket) -> gen_server:cast(Pid, {leave, User, Socket}).
speak(Pid, User, Socket, Msg) -> gen_server:cast(Pid, {speak, User, Socket, Msg}).
online(Pid, Socket) -> gen_server:cast(Pid, {online, Socket}).
offline(Pid, Socket) -> gen_server:cast(Pid, {offline, Socket}).

%% 回调函数
init([GroupId, GroupName, UserLists, SocketLists]) ->
    {ok, #group{id=GroupId, name=GroupName, users=UserLists, sockets=SocketLists}}.

handle_call(Commend, _From, Group) ->
    {reply, Commend, Group}.

handle_cast(Msg, Group) ->
    io:format("Unknow message ~p, ~n", [Msg]),
    {noreply, Group}.

handle_info({join, User, Socket}, Group) ->
    case lists:member(User, Group#group.users) of
        true ->
            gen_tcp:send(Socket, term_to_binary("you are in this Group: ~p already!~n", [Group#group.name])),
            {noreply, Group};
        false ->
            io:format("~p join group: ~p success!~n", [User, Group#group.name]),
            NewUserList = [User | Group#group.users],
            NewSocketList = [Socket | Group#group.sockets],
            io:format("Group user list: ~p~n", [NewUserList]),
            io:format("Group socket list: ~p~n", [NewSocketList]),
            {noreply, Group#group{users=NewUserList, sockets=NewSocketList}}
    end;
handle_info({leave, User, Socket}, Group) ->
    case lists:member(User, Group#group.users) of
        false ->
            gen_tcp:send(Socket, term_to_binary("you are not in this Group: ~p~n!", [Group#group.name]));
        true ->
            io:format("~p leave group: ~p success!~n", [User, Group#group.name]),
            NewUserList = lists:delete(User, Group#group.users),
            NewSocketList = lists:delete(Socket, Group#group.sockets),
            io:format("Group user list: ~p~n", [NewUserList]),
            io:format("Group socket list: ~p~n", [NewSocketList])
    end,
    {noreply, Group#group{users=lists:delete(User, Group#group.users), sockets=lists:delete(Socket, Group#group.sockets)}};
handle_info({speak, User, Socket, Msg}, Group) ->
    case lists:member(User, Group#group.users) of
        true ->
            io:format("~p group talk: ~p~n", [User, Group#group.name]),
            io:format("Group user list: ~p~n", [Group#group.users]),
            io:format("Group socket list: ~p~n", [Group#group.sockets]),
            boardcast(Group#group.sockets, Socket, User, Msg, Group#group.name);
        false ->
            gen_tcp:send(Socket, term_to_binary("you are not in this group"))
    end,
    {noreply, Group};
handle_info({online, Socket}, Group) ->
    io:format("online ~p~n", [Group#group.name]),
    io:format("Socketlist in group is: ~p~n", [[Socket| Group#group.sockets]]),
    {noreply, Group#group{sockets=[Socket| Group#group.sockets]}};
handle_info({offline, Socket}, Group) ->
    io:format("offline ~p~n", [Group#group.name]),
    io:format("Socketlist in group is: ~p~n", [lists:delete(Socket, Group#group.sockets)]),
    {noreply, Group#group{sockets=lists:delete(Socket, Group#group.sockets)}};
handle_info(Msg, Group) ->
    io:format("Unknow message ~p, ~n", [Msg]),
    {noreply, Group}.

%% 终结
terminate(normal, _Socket) ->
    io:format("group disband now~n"),
    ok.

%% 热更新
code_change(_OldVsn, Socket, _Extra) ->
    {ok, Socket}.

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
