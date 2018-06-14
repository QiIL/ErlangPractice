-module(exception).
-compile(export_all).

throws(F) ->
    try F() of
        _ -> ok
    catch
        Throw -> {throw, caught, Throw}
    end.

error(F) ->
    try F() of
        _ -> ok
    catch
        error:Error -> {error, caught, Error}
    end.

exit(F) ->
    try F() of
        _ -> ok
    catch
        exit:Exit -> {exit, caught, Exit}
    end.

sword(1) -> throw(slice);
sword(2) -> erlang:error(cut_arm);
sword(3) -> erlang:exit(cut_leg);
sword(4) -> throw(punch);
sword(5) -> erlang:exit(cross_bridge).

talk() -> "blash blash".

black_knight(Attack) when is_function(Attack, 0) ->
    try Attack() of
        _ -> "None shell pass."
    catch
        throw:slice -> "It's but a scratch.";
        error:cut_arm -> "I've had worse";
        exit:cut_leg -> "Come on you pansy!";
        exit:cross_bridge -> "oh it's a stupid move";
        _:_ -> "Just a flesh wound"
    end.

%% try of之间多个表达式
whoa() ->
    try
        talk(),
        _Knight = "Node shall pass!",
        _Doubles = [N*2 || N <- lists:seq(1, 100)],
        throw(up),
        _WillReturnThis = tequila
    of
        tequila -> "Hey, this worked!"
    catch
        Exception:Reason -> {caught, Exception, Reason}
    end.

im_impressed() ->
    try
        talk(),
        _Knight = "Node shall pass!",
        _Doubles = [N*2 || N <- lists:seq(1, 100)],
        throw(up),
        _WillReturnThis = tequila
    catch
        Exception:Reason -> {caught, Exception, Reason}
    end.

catcher(X, Y) ->
    case catch X/Y of
        {'EXIT', {badarith, _}} -> "uh oh";
        N -> N
    end.

