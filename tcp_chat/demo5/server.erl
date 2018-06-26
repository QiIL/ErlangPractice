-module(server).
-compile(export_all).

start() ->
    {ok, Listen} = gen_tcp:listen(4000, [binary, {active, true}]),
    ets:new(users, [set, named_table]),
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
                {new, User, Pass} ->
                    Jenkins ! {new, User, Pass};
                {change_pass, User, OldPass, NewPass} ->
                    Jenkins ! {change_pass, User, OldPass, NewPass};
                {talk, User, Msg} ->
                    Jenkins ! {broadcast, Socket, User ++ "：" ++ Msg};
                {quit, User} ->
                    Jenkins ! {quit, Socket, User}
            end,
            loop(Jenkins, Socket);
        {tcp_closed, _Socket} ->
            io:format("tcp server close!~n")
    end.
