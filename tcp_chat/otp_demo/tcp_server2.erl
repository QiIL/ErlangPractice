-module(tcp_server2).
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
    {ok, Pid} = msg_server:new(Socket),
    gen_tcp:controlling_process(Socket, Pid).