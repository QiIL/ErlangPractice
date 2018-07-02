-module(trade_fsm).
% -behaviour(gen_fsm).

% %% 公共api
% -export([start/1, start_link/1, trade/2, accept_trade/1, make_offer/2, retract_offer/2, ready/1, cancel/1]).

% %% gen_fsm 回调函数
% -export([init/1])