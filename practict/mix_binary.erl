-module(mix_binary).
-compile(export_all).

b_reverse(<<>>, Bin2) -> Bin2;
b_reverse(Bin, Bin2) ->
    <<R:8, Rest/binary>> = Bin,
    Bin3 = <<R:8, Bin2/binary>>,
    b_reverse(Rest, Bin3).
    
do_mix(N) ->
    case N of
        0 -> ok;
        _ -> 
            mix_binary(<<1, 2, 3, 4, 5, 6>>, 1, 0),
            do_mix(N-1)
    end.

mix_binary(Binary, K, L) ->
    mix_binary(Binary, K, L, <<>>).
mix_binary(<<>>, _, _, Result) ->
    Result;
mix_binary(Binary, K, L, Result) ->
    case L == 0 of
        true ->
            % io:format("L == 0, Binary is : ~p, Result is ~p~n", [Binary, Result]),
            <<R:8, Rest/binary>> = Binary,
            N = bnot R,
            Bin = <<Result/binary, N>>,
            mix_binary(Rest, K, K, Bin);
        false ->
            % io:format("L == ~p, Binary is : ~p, Result is ~p~n", [L, Binary, Result]),
            <<R:8, Rest/binary>> = Binary,
            Bin = <<Result/binary, R>>,
            mix_binary(Rest, K, L-1, Bin)
    end.

%%% 去掉没用的东西的版本
do_mix2(0) ->
    ok;
do_mix2(N) ->
    mix_binary2(<<1, 2, 3, 4, 5, 6>>, 1, 0),
    do_mix2(N-1).

mix_binary2(Binary, K, L) ->
        mix_binary(Binary, K, L, <<>>).
mix_binary2(<<>>, _, _, Result) ->
    Result;
mix_binary2(<<R:8, Rest/binary>>, K, 0, Result) ->
    mix_binary2(Rest, K, K, <<Result/binary, bnot R>>);
mix_binary2(<<R:8, Rest/binary>>, K, L, Result) ->
    mix_binary2(Rest, K, L-1, <<Result/binary, R>>).
