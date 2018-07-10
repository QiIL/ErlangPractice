-module(server).
-export([start/0]).

%%打开服务器，每一个连接都新建一个进程来循环等待Socket的消息
start() ->
    {ok, Listen} = gen_tcp:listen(4000, [binary, {active, true}]),
    ets:new(users, [set, public, named_table]),
    ets:insert(users, [
        {admin, admin, false},
        {qill, 11, false},
        {user1, 11, false},
        {user2, 11, false},
        {user3, 11, false},
        {user4, 11, false}
    ]),
    %% socket表
    ets:new(socket_list, [set, public, named_table]),
    ets:insert(socket_list, {sockets, []}),
    pre_loop(Listen).

%% 新建连接需要告诉其他进程增加了一个进程
pre_loop(Listen) ->
    io:format("ok to pre loop~n"),
    {ok, Socket} = gen_tcp:accept(Listen),
    SocketList = ets:lookup_element(socket_list, sockets, 2),
    io:format("SocketList is: ~p~n", [SocketList]),
    ets:insert(socket_list, {sockets, [Socket | SocketList]}),
    spawn(fun() -> pre_loop(Listen) end),
    loop(Socket).

loop(Socket) ->
    receive
        {tcp, Socket, Bin} ->
            Str = binary_to_term(Bin),
            io:format("Server (unpacked) ~p~n", [Str]),
            case Str of
                {talk, User, Msg} ->
                    broadcast(ets:lookup_element(socket_list, sockets, 2), Socket, User, Msg)
            end,
            loop(Socket);
        {tcp_closed, _Socket} ->
            io:format("tcp server close!~n")
    end.
    
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