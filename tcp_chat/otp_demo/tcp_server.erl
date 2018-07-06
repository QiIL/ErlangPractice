-module(tcp_server).
-export([
    start_link/0, showets/0,
    init/1, handle_call/3, handle_cast/2,
    handle_info/2, terminate/2, code_change/3
]).
-behaviour(gen_server).

start_link() -> 
    {ok, Listen} = gen_tcp:listen(4000, [binary, {active, true}]),
    ets:new(users, [set, public, named_table]),
    ets:insert(users, [
        {admin, admin, false},
        {qill, 11, false},
        {user1, 11, false},
        {user2, 11, false},
        {user3, 11, false},
        {user4, 11, false}
    ]),
    %% 群组表
    ets:new(groups, [ordered_set, public, named_table]),
    {ok, NormalPid} = groups:new(1, normal_people, qill),
    ets:insert(groups, [
        {1, normal_people, qill, NormalPid}
    ]),
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