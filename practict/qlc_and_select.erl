-module(mmnesia).
-export([
    do_test/1, do_test2/1,
    search/3, search/4, last_tab/1, write/1
]).
-include_lib("stdlib/include/qlc.hrl").
-include("../tcp_chat/otp_demo/include/records.hrl").
%% compare qlc and mnesia:select
do_test(0) -> ok;
do_test(N) ->
    test(),
    do_test(N-1).

test() ->
    search_qlc(qlc:q([{G#chat_group.id, G#chat_group.name} || 
                        G <- mnesia:table(group),
                        G#chat_group.owner =:= qill
                ])).

do_test2(0) -> ok;
do_test2(N) ->
    test2(),
    do_test2(N-1).

test2() ->
    F = fun() ->
        mnesia:select(chat_user,[{#chat_user{_='_'}, [], ['$_']}])
    end,
    mnesia:transaction(F).

search(Tab, R, Guards) ->
    F = fun() ->
        mnesia:select(Tab, [{R, Guards, ['$_']}])
    end,
    {atomic, Val} = mnesia:transaction(F),
    Val.

search(Tab, R, Guards, Field) ->
    F = fun() ->
        mnesia:select(Tab, [{R, Guards, Field}])
    end,
    {atomic, Val} = mnesia:transaction(F),
    Val.

last_tab(Tab) ->
    {atomic, Val} = mnesia:transaction(fun() -> mnesia:last(Tab) end),
    Val.

write(Rec) ->
    mnesia:transaction(fun() -> mnesia:write(Rec) end).

%% give a qlc
search_qlc(Q) ->
    F = fun() -> qlc:e(Q) end,
    {atomic, Val} = mnesia:transaction(F),
    Val.
