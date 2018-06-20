-module(socket_example).
-compile(export_all).

start_seq_server() ->
    {ok, Listen} = gen_tcp:listen(4000, [binary, {active, true}]),
    seq_loop(Listen).

seq_loop(Listen) ->
    {ok, Socket} = gen_tcp:accept(Listen),
    loop(Socket),
    seq_loop(Listen).

loop(Socket) ->
    receive
        {tcp, Socket, Bin} ->
            io:format("Server received binary = ~p~n", [Bin]),
            Str = binary_to_term(Bin),
            io:format("Server (unpacked) ~p~n", [Str]),
            gen_tcp:send(Socket, term_to_binary(Str)),
            loop(Socket);
        {tcp_closed, Socket} ->
            io:format("tcp server close!~n")
    end.

nano_client_eval(Str) ->
    {ok, Socket} = gen_tcp:connect("localhost", 4000, [binary]),
    ok = gen_tcp:send(Socket, term_to_binary(Str)),
    receive
        {tcp, Socket, Bin} ->
            io:format("Client receive binary = ~p~n", [Bin]),
            Val = binary_to_term(Bin),
            io:format("Client result ~p~n", [Val]),
            gen_tcp:close(Socket)
    end.