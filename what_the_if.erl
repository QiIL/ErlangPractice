-module(what_the_if).
-compile([debug_info, export_all]).

heh_fine() -> 
    if 1 =:= 1 ->
        works
end,
    if 1 =:= 2; 1 =:= 1 ->
        works
end,
    if 1 =:= 2, 1 =:= 1 ->
        fails;
    true -> 
        forbitten_fails
end.

oh_god(N) ->
    if N =:= 2 -> might_succees;
    true -> always_does %% 这是erlang if 的 ‘else’。
end.

help_me(Animal) ->
    Talk = if Animal == cat -> "meow";
        Animal == beef -> "bark";
        Animal == dog -> "bark";
        Animal == tree -> "bark";
        true -> "fgdadfgna"
    end,
    {Animal, "says " ++ Talk ++ "!"}.
