-module(self).
-compile(export_all).

init() ->
    ets:new(self_table, [set, named_table]),
    ets:insert(self_table, {aaa, self()}).

check() ->
    ets:match(self_table, {'$1', '$2'}).
