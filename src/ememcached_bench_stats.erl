-module(ememcached_bench_stats).

%% API
-export([init/0, init_counter/2, get_counter/1, get_counters/1, update_counter/2]).

-define(TAB, bench_stats).

%% ===================================================================
%% API
%% ===================================================================
init() ->
    ets:new(?TAB,
        [
            set,
            public,
            named_table,
            {write_concurrency, true},
            {read_concurrency, false}
        ]).

init_counter(Key, SenarioName) ->
    %% Key = SenarioName + Pid
    ets:insert(?TAB, {Key, SenarioName, 0}).

update_counter(Key, Count) ->
    ets:update_counter(?TAB, Key, {3, Count}).

get_counter(Key) ->
    case ets:lookup(?TAB, Key) of
        [] ->
            notfound;
        [Entry] ->
            {ok, Entry}
    end.

get_counters(SenarioName) ->
    case ets:match(?TAB, {'_', SenarioName, '$1'}) of
        [] ->
            notfound;
        Result ->
            [N || [N] <- Result]
    end.
    
