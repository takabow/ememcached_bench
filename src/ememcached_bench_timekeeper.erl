-module(ememcached_bench_timekeeper).

%% API
-export([start_link/0, init/0]).

%% ===================================================================
%% API
%% ===================================================================
start_link() ->
    spawn_link(?MODULE, init, []).

%% ===================================================================
%% Callbacks
%% ===================================================================
init() ->
    ready().

%% ===================================================================
%% Internal
%% ===================================================================
ready() ->
    receive
        go ->
            run(0, erlang:now())
    end.

run(LastCount, LastTime) ->
    timer:sleep(10000), %% ざっくり10秒毎に表示

    CurrentTime = erlang:now(),
    Counters = ememcached_bench_stats:get_counters(s1), %% s1 means scenario1
    Sum = lists:sum(Counters),
    DiffTime = timer:now_diff(CurrentTime, LastTime),
    
    io:format("~p qps~n",[(Sum - LastCount)/(DiffTime/1000000)]),

    run(Sum, erlang:now()).

