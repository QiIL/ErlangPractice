-module(server).
-compile(export_all).

start() ->
    {ok, Listen} = gen_tcp:listen(4000, [binary, {active, true}]),
    Jenkins = spawn(jenkins, manager, []),
    pre_loop(Listen, Jenkins).

pre_loop(Listen, Jenkins) ->
    {ok, Socket} = gen_tcp:accept(Listen),
    spawn(fun() -> pre_loop(Listen, Jenkins) end),
    loop(Jenkins, Socket).

loop(Jenkins, Socket) ->
    receive
        {tcp, Socket, Bin} ->
            Str = binary_to_term(Bin),
            io:format("Server (unpacked) ~p~n", [Str]),
            case Str of
                {new, Username, Pass} ->
                    Jenkins ! {new, Username, Pass};
                {change_pass, Username, OldPass, NewPass} ->
                    Jenkins ! {change_pass, Username, OldPass, NewPass};
                {talk, Username, Msg} ->
                    Jenkins ! {broadcast, Socket, Username ++ "：" ++ Msg};
                {quit, Username} ->
                    Jenkins ! {quit, Socket, Username}
            end,
            loop(Jenkins, Socket);
        {tcp_closed, _Socket} ->
            io:format("tcp server close!~n")
    end.
