-module(ppool_sup).
-export([start_link/3, init/1]).
-behaviour(supervisor).

start_link(Name, Litmit, MFA) ->
    supervisor:start_link(?MODULE, {Name, Litmit, MFA}).

init({Name, Litmit, MFA}) ->
    MaxRestart = 1,
    MaxTime = 3600,
    {ok, {{one_for_all, MaxRestart, MaxTime},
          [{serv,
            {ppool_serv, start_link, [Name, Litmit, self(), MFA]},
            permanent,
            5000, % 关闭时间
            worker,
            [ppool_serv]
           }]
         }
    }.