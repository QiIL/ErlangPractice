-module(mmnesia).
-export([
    init/0, start/0, reset_dbs/0,
    search/4, search/3, test/0, do_test/1,
    do_test2/1, test2/0, search_qlc/1,
    write/1, last_tab/1, delete_tabs/0, man_db/0
]).
-include_lib("stdlib/include/qlc.hrl").
-include("../include/records.hrl").

init() ->
    mnesia:create_schema([node()]),
    mnesia:start(),
    mnesia:create_table(chat_user, [{disc_copies, [node()]}, {attributes, record_info(fields, chat_user)}]),
    mnesia:create_table(chat_group, [{type, ordered_set}, {disc_copies, [node()]}, {attributes, record_info(fields, chat_group)}]),
    mnesia:create_table(chat_record, [{type, ordered_set}, {disc_copies, [node()]}, {attributes, record_info(fields, chat_record)}]),
    mnesia:stop().

delete_tabs() ->
    mnesia:delete_table(chat_user),
    mnesia:delete_table(chat_group),
    mnesia:delete_table(chat_record).

start() ->
    mnesia:start(),
    mnesia:wait_for_tables([chat_user,chat_group,chat_record], 20000),
    Pid = spawn_link(?MODULE, man_db, []),
    register(man_db, Pid).

reset_dbs() ->
    mnesia:clear_table(chat_user),
    mnesia:clear_table(chat_group),
    mnesia:clear_table(chat_record),
    F = fun() ->
		lists:foreach(fun mnesia:write/1, example_tables())
	end,
    mnesia:transaction(F).

example_tables() ->
    [%% The user table
     {chat_user, qill, 11},
     {chat_user, user1, 11},
     {chat_user, user2, 11},
     {chat_user, user3, 11},
     {chat_user, admim, admin},
     %% The group table
     {chat_group, 1, normalpeople, qill, [qill]},
     {chat_group, 2, users, user1, [user1, user2, user3, admim]},
     %% the chat record table
     {chat_record, 1, qill, broadcast, world, 1531155082000, "the first msg"},
     {chat_record, 2, qill, broadcast, world, 1531155083000, "second msg now"},
     {chat_record, 3, user1, whisper, qill, 1531155084000, "user1 whisper with qill"},
     {chat_record, 3, user2, group_talk, users, 1531155085000, "user2 msg in users group"}
    ].

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

get_time() ->
    {M, S, _} = os:timestamp(),
    (M * 1000000 + S) * 1000.

man_db() ->
    receive
        {save_rec, Type, User, Target, Msg} ->
            io:format("Type: ~p, user: ~p, Target: ~p, Msg: ~p~n", [Type, User, Target, Msg]),
            RecId = last_tab(chat_record) + 1,
            write({chat_record, RecId, User, Type, Target, get_time(), Msg}),
            man_db()
    end.