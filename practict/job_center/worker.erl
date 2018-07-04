-module(worker).
-compile(export_all).

work() ->
    receive
        {do_job, JobNum} ->
            case job_center:work_wanted(JobNum) of
                {_JobNumber, Fun} -> 
                    Fun(), 
                    ok;
                Reason -> Reason
            end;
        show_jobs ->
            io:format("~p~n", [job_center:show_jobs()]),
            work();
        statistics ->
            io:format("~p~n", [job_center:statistics()]),
            work()
    end.
