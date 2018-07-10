-module(tcp_server).
-export([
    start_link/0, showets/0,
    init/1, handle_call/3, handle_cast/2,
    handle_info/2, terminate/2, code_change/3
]).
-include("../include/records.hrl").
-behaviour(gen_server).

%% 启动载入各种表
start_link() -> 
    %% 打开数据库
    mmnesia:start(),
    {ok, Listen} = gen_tcp:listen(4000, [binary, {active, true}]),
    %% 用户session
    ets:new(users, [set, public, named_table]),
    ChatUsers = mmnesia:search(chat_user, #chat_user{_='_'}, []),
    init_users(ChatUsers),
    %% 群组表, 建立在线数据
    ets:new(groups, [public, named_table]),
    Groups = mmnesia:search(chat_group, #chat_group{_='_'}, []),
    create_group_process(Groups),
    %% socket表
    ets:new(socket_list, [set, public, named_table]),
    ets:insert(socket_list, {sockets, []}),
    gen_server:start_link(?MODULE, [Listen], []).

showets() -> gen_server:call(?MODULE, showets).

init([Listen]) -> 
    self() ! wait_connect,
    {ok, Listen}.

handle_call(Commend, _From, Listen) ->
    io:format("Unkown commend: ~p~n", Commend),
    {reply, Commend, Listen}.

handle_cast(_Msg, Listen) ->
    {noreply, Listen}.

handle_info(wait_connect, Listen) ->
    {ok, Socket} = gen_tcp:accept(Listen),
    {ok, Pid} = msg_server:new(Socket),
    gen_tcp:controlling_process(Socket, Pid),
    self() ! wait_connect,
    {noreply, Listen};
handle_info(Msg, Listen) ->
    io:format("Unexpected Msg in tcp server: ~p~n", [Msg]),
    {noreply, Listen}.

terminate(normal, _Listen) ->
    io:format("tcp server close now~n"),
    ok.

code_change(_OldVsn, Listen, _Extra) ->
    {ok, Listen}.

create_group_process([]) -> ok;
create_group_process([{chat_group, Gid, GName, _Owner, Members} | T]) ->
    {ok, NormalPid} = groups:new(Gid, GName, Members),
    ets:insert(groups, {Gid, GName, Members, NormalPid}),
    create_group_process(T).

init_users([]) -> ok;
init_users([{chat_user, Username, Pass} | T]) ->
    ets:insert(users, {Username, Pass, false}),
    init_users(T).