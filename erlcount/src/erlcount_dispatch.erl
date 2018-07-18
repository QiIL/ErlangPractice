-module(erlcount_dispatch).
-behaviour(gen_fsm).
-export([start_link/0, complete/4]).
-export([init/1, dispatching/2, listening/2, handle_event/3,
    handle_sync_event/4, handle_info/3, terminate/3, code_change/4
]).
-define(POOL, erlcount).
-record(data, {regex=[], refs=[]}).

start_link() ->
    gen_fsm:start_link(?MODULE, [], []).

complete(Pid, Regex, Ref, Count) ->
    gen_fsm:send_all_state_event(Pid, {complete, Regex, Ref, Count}).

init([]) ->
    {ok, Re} = application:gen_env(regex),
    {ok, Dir} = application:get_env(directory),
    {ok, Maxfiles} = application:get_env(max_files),
    ppool:start_pool(?POOL, Maxfiles, {erlcount_counter, start_link, []}),
    case lists:all(fun valid_regex/1, Re) of
        true ->
            self() ! {start, Dir},
            {ok, diapatching, #data{regex=[{R,0} || R<-Re]}};
        false ->
            {stop, invalid_regex}
    end.