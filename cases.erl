-module(cases).
-compile([export_all]).

insert(X, []) ->
    [X];
insert(X, Set) ->
    case list:member(X, Set) of
        true -> Set;
        false -> [X | Set]
    end.

%% case语句
beach(Tempeture) ->
    case Tempeture of
        {celsius, N} when N >= 20, N =< 45 ->
            'favorable';
        {kelvin, N} when N >= 293, N =< 318 ->
            'scientifically favorable';
        {fahrenheit, N} when N >= 68, N =< 113 ->
            'favorable in the US';
        _ ->
            'avoid beach'
    end.

%% 模式匹配写法
beach2({celsius, N}) when N >= 20, N =< 45 ->
    'favorable';
beach2({kelvin, N}) when N >= 293, N =< 318 ->
    'scientifically favorable';
beach2({fahrenheit, N}) when N >= 68, N =< 113 ->
    'favorable in the US';
beach2(_) ->
    'avoid beach'.
