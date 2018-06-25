-module(server).
-export([start/0]).
-vsn("1.0.0").

%%打开服务器，每一个连接都新建一个进程来循环等待Socket的消息
start() ->
    {ok, Listen} = gen_tcp:listen(4000, [binary, {active, true}]),
    Postman = spawn(postman, post_feature, [[], []]),
    pre_loop(Listen, Postman).

%% 新建连接需要告诉其他进程增加了一个进程
pre_loop(Listen, Postman) ->
    {ok, Socket} = gen_tcp:accept(Listen),
    spawn(fun() -> pre_loop(Listen, Postman) end),
    loop(Postman, Socket).

loop(Postman, Socket) ->
    receive
        {tcp, Socket, Bin} ->
            Str = binary_to_term(Bin),
            io:format("Server (unpacked) ~p~n", [Str]),
            case Str of
                {online, Nick} ->
                    Postman ! {online, Socket, Nick};
                {talk, Nick, Msg} ->
                    Postman ! {broadcast, Socket, Nick ++ ":" ++ Msg};
                {quit, Nick} -> 
                    Postman ! {quit, Socket, Nick}
            end,
            loop(Postman, Socket);
        {tcp_closed, _Socket} ->
            io:format("tcp server close!~n")
    end.
    