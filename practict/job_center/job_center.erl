-module(job_center).
-export([
    start_link/0, stop/0, add_job/1,
    work_wanted/1, job_done/1, statistics/0, 
    show_jobs/0, init/1,
    handle_call/3, handle_cast/2, handle_info/2,
    terminate/2, code_change/3
]).
-behaviour(gen_server).

start_link() -> gen_server:start_link({local, ?MODULE}, ?MODULE, [], []).
stop() -> gen_server:call(?MODULE, stop).
add_job(Fun) -> gen_server:call(?MODULE, {add_job, Fun}).
work_wanted(JobNumber) -> gen_server:call(?MODULE, {work_wanted, JobNumber}).
job_done(JobNumber) -> gen_server:call(?MODULE, {done, JobNumber}).
statistics() -> gen_server:call(?MODULE, statistics).
show_jobs() -> gen_server:call(?MODULE, show_jobs).

init([]) -> {ok, ets:new(?MODULE, [ordered_set])}.

handle_call({add_job, Fun}, _From, JobTable) ->
    LastNum = 
        case ets:last(JobTable) of
            '$end_of_table' -> 0;
            Num -> Num
        end,
    ets:insert(JobTable, {LastNum + 1, Fun, waiting}),
    Reply = ets:last(JobTable),
    {reply, Reply, JobTable};

handle_call({work_wanted, JobNumber}, _From, JobTable) ->
    Reply = 
        case ets:lookup(JobTable, JobNumber) of
            [] -> job_is_not_exit;
            [{_, Fun, waiting}] ->
                ets:insert(JobTable, {JobNumber, Fun, doing}),
                {JobNumber, Fun};
            [_] -> you_can_not_want_this_job
        end,
    {reply, Reply, JobTable};

handle_call({done, JobNumber}, _From, JobTable) ->
    Reply = 
        case ets:lookup(JobTable, JobNumber) of
            [] -> job_is_not_exit;
            [{_, Fun, doing}] -> 
                ets:insert(JobTable, {JobNumber, Fun, done}),
                mission_complete;
            [{_, _, waiting}] ->
                job_have_not_distribute
        end,
    {reply, Reply, JobTable};

handle_call(statistics, _From, JobTable) ->
    Waiting = length(ets:match(JobTable, {'_', '_', waiting})),
    Doing = length(ets:match(JobTable, {'_', '_', doing})),
    Done = length(ets:match(JobTable, {'_', '_', done})),
    Reply = {{waiting, Waiting}, {doing, Doing}, {done, Done}},
    {reply, Reply, JobTable};

handle_call(show_jobs, _From, JobTable) ->
    Reply = ets:match_object(JobTable, {'$1', '$2', '$3'}),
    {reply, Reply, JobTable};

handle_call(stop, _From, JobTable) ->
    {stop, normal, stopped, JobTable}.

handle_cast(_Msg, State) -> {noreply, State}.
handle_info({'DOWN', _Ref, process, _Pid, _Why}, State) -> 
    io:format("lala is down ~n"),
    {noreply, State};
handle_info(_Info, State) -> {noreply, State}.
terminate(_Reason, _State) -> ok.
code_change(_OldVsn, State, _Extra) -> {ok, State}.