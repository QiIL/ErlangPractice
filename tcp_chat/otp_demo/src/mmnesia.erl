-module(mmnesia).
-export([
    start_link/0, stop/0, init_table/0,
    reset_dbs/0, delete_tabs/0, test_search/0,
    save_rec/4, delete_recs/2, search/4, change_pass/2,
    create_group/2, add_group_member/2, minus_group_member/2
]).
-export([
    init/1, handle_call/3, handle_cast/2,
    handle_info/2, terminate/2, code_change/3  
]).
-include("../include/records.hrl").
-behaviour(gen_server).
-define(TIMEOUT, 1000 * 60 * 10). % 10分钟清一次聊天记录

start_link() -> gen_server:start_link({local, ?MODULE}, ?MODULE, [], []).
stop() -> gen_server:call(?MODULE, stop).

init([]) ->
    mnesia:start(),
    mnesia:wait_for_tables([chat_user,chat_group,chat_record], 20000),
    erlang:send_after(?TIMEOUT, self(), clean_record),
    {ok, []}.

init_table() -> gen_server:call(?MODULE, init_table).
delete_tabs() -> gen_server:call(?MODULE, delete_tabs).
reset_dbs() -> gen_server:call(?MODULE, reset_dbs).
save_rec(Type, User, Target, Msg) -> gen_server:call(?MODULE, {save_rec, Type, User, Target, Msg}).
change_pass(Username, NewPass) -> gen_server:call(?MODULE, {change_pass, Username, NewPass}).
test_search() -> gen_server:call(?MODULE, test_search).
create_group(Gname, Owner) -> gen_server:call(?MODULE, {create_group, Gname,  Owner}).
add_group_member(Gid, Username) -> gen_server:call(?MODULE, {add_group_member, Gid,  Username}).
minus_group_member(Gid, Username) -> gen_server:call(?MODULE, {minus_group_member, Gid, Username}).


%% handle_call
handle_call(init_table, _From, State) ->
    mnesia:create_schema([node()]),
    mnesia:start(),
    mnesia:create_table(chat_user, [{disc_copies, [node()]}, {attributes, record_info(fields, chat_user)}]),
    mnesia:create_table(chat_group, [{type, ordered_set}, {disc_copies, [node()]}, {attributes, record_info(fields, chat_group)}]),
    mnesia:create_table(chat_record, [{type, ordered_set}, {disc_copies, [node()]}, {attributes, record_info(fields, chat_record)}]),
    mnesia:stop(),
    {reply, ok, State};

handle_call(delete_tabs, _From, State) ->
    mnesia:delete_table(chat_user),
    mnesia:delete_table(chat_group),
    mnesia:delete_table(chat_record),
    {reply, ok, State};
    
handle_call(reset_dbs, _From, State) ->
    mnesia:clear_table(chat_user),
    mnesia:clear_table(chat_group),
    mnesia:clear_table(chat_record),
    F = fun() ->
		lists:foreach(fun mnesia:write/1, example_tables())
	end,
    mnesia:transaction(F),
    {reply, ok, State};

handle_call({save_rec, Type, User, Target, Msg}, _From, State) ->
    io:format("Type: ~p, user: ~p, Target: ~p, Msg: ~p~n", [Type, User, Target, Msg]),
    RecId = last_tab(chat_record),
    case RecId of
        '$end_of_table' ->
            write({chat_record, 1, User, Type, Target, get_time(), Msg});
        _ ->
            write({chat_record, RecId+1, User, Type, Target, get_time(), Msg})
    end,
    {reply, save_ok, State};

handle_call(test_search, _From, State) ->
    R = #chat_record{id='$1', time='$2', _='_'},
    Guards = [{'<', '$2', 1531155084}],
    Recs = search(chat_record, R, Guards, ['$1']),
    NewRecs = lists:map(fun(Key) -> {chat_record, Key} end, Recs),
    {reply, NewRecs, State};
    
handle_call({change_pass, Username, NewPass}, _From, State) ->
    write({chat_user, Username, NewPass}),
    {reply, ok, State};

handle_call({create_group, Gname, Owner}, _From, State) ->
    Id = last_tab(group),
    write({chat_group, Id+1, Gname, Owner, [Owner]}),
    {reply, create_group_ok, State};

handle_call({add_group_member, Gid, Username}, _From, State) ->
    [{_, _, Gname, Owner, Members}] = search(chat_group, #chat_group{id='$1', _='_'}, [{'==', '$1', Gid}], ['$_']),
    write({chat_group, Gid, Gname, Owner, [Username | Members]}),
    {reply, add_member_ok, State};

handle_call({minus_group_member, Gid, Username}, _From, State) ->
    [{_, _, Gname, Owner, Members}] = search(chat_group, #chat_group{id='$1', _='_'}, [{'==', '$1', Gid}], ['$_']),
    write({chat_group, Gid, Gname, Owner, lists:delete(Username, Members)}),
    {reply, add_member_ok, State};

handle_call(stop, _From, State) ->
    {stop, normal, State}.

%% handle cast
handle_cast(_Msg, State) ->
    {noreply, State}.

%% handle info
handle_info(clean_record, State) ->
    io:format("delete the record~n"),
    TenMinAgo = get_time() - 10 * 60,
    R = #chat_record{id='$1', time='$2', _='_'},
    Guards = [{'<', '$2', TenMinAgo}],
    Recs = search(chat_record, R, Guards, ['$1']),
    delete_recs(chat_record, Recs),
    erlang:send_after(?TIMEOUT, self(), clean_record),
    {noreply, State};

handle_info(_Msg, State) ->
    {noreply, State}.

terminate(normal, _State) ->
    mnesia:stop(),
    ok.

%% 热更新
code_change(_OldVsn, Socket, _Extra) ->
    {ok, Socket}.

example_tables() ->
    [%% The user table
     {chat_user, qill, 11},
     {chat_user, user1, 11},
     {chat_user, user2, 11},
     {chat_user, user3, 11},
     {chat_user, admin, admin},
     %% The group table
     {chat_group, 1, normalpeople, qill, [qill]},
     {chat_group, 2, users, user1, [user1, user2, user3, admim]},
     %% the chat record table
     {chat_record, 1, qill, broadcast, world, get_time() - 11 * 60, "the first msg"},
     {chat_record, 2, qill, broadcast, world, get_time() - 9 * 60, "second msg now"},
     {chat_record, 3, user1, whisper, qill, get_time() - 5 * 60, "user1 whisper with qill"},
     {chat_record, 4, user2, group_talk, users, get_time(), "user2 msg in users group"}
    ].

%% 获取某表的最后一个元素
last_tab(Tab) ->
    {atomic, Val} = mnesia:transaction(fun() -> mnesia:last(Tab) end),
    Val.

%% 表搜索
search(Tab, R, Guards, Field) ->
    F = fun() ->
        mnesia:select(Tab, [{R, Guards, Field}])
    end,
    {atomic, Val} = mnesia:transaction(F),
    Val.

%% 写表
write(Rec) ->
    mnesia:transaction(fun() -> mnesia:write(Rec) end).

%% 获取时间戳
get_time() ->
    {M, S, _} = os:timestamp(),
    M * 1000000 + S.

%% 删除表
delete_recs(_Tab, []) -> ok;
delete_recs(Tab, Recs) ->
    NewRecs = lists:map(fun(Key) -> {Tab, Key} end, Recs),
    F = fun() ->
        lists:foreach(fun mnesia:delete/1, NewRecs)
    end,
    mnesia:transaction(F).
