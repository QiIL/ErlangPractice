-module(client).
-export([
    start/0, stop/0,
    talk/1, quit/0,
    init/1, handle_call/3, handle_cast/2,
    handle_info/2, terminate/2, code_change/3
]).
-behaviour(gen_server).

start() -> gen_server:start_link({local, ?MODULE}, ?MODULE, [], []).
stop() -> gen_server:call(?MODULE, stop).

% login() -> gen_server:call(?MODULE, login).
talk(Msg) -> gen_server:call(?MODULE, {talk, Msg}).
quit() -> gen_server:call(?MODULE, quit).

init([]) -> 
    gen_tcp:connect("192.168.15.98", 4000, [binary, {nodelay, true}]).

%% 同步调用
handle_call({tcp, _Socket, Bin}, _From, Socket) ->
    {reply, binary_to_term(Bin), Socket};
handle_call(quit, _From, Socket) ->
    gen_tcp:send(Socket, term_to_binary({talk, Msg})),
    