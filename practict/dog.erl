-module(dog).
-compile(export_all).

start() ->
    spawn(fun() -> bark() end).

squirrel(Pid) -> Pid ! squirrel.

pet(Pid) -> Pid ! pet.

bark() ->
    io:format("Dog say: BARK! BARK! ~n"),
    receive
        pet ->
            wave_tail();
        _ ->
            io:format("Dog is confuse"),
            bark()
    after 2000 ->
        bark()
    end.

wave_tail() ->
    io:format("Dog wag it's tail~n"),
    receive
        pet ->
            sit();
        _ ->
            io:format("Dog is confuse~n"),
            wave_tail()
    after 3000 ->
        bark() 
    end.

sit() ->
    io:format("Dog is sitting. Gooooooooooood boy!~n"),
    receive
        squirrel ->
            bark();
        _ ->
            io:format("Dog is confuse~n"),
            sit()
    end.