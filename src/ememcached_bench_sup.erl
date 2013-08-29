-module(ememcached_bench_sup).

%% API
-export([start_link/3, init/4]).

%% ===================================================================
%% API
%% ===================================================================
start_link(Address, Port, Concurrency) ->
    proc_lib:start_link(?MODULE, init, [self(), Address, Port, Concurrency]).

%% ===================================================================
%% Callbacks
%% ===================================================================
init(Parent, Address, Port, Concurrency) ->
    register(ememcached_bench_sup, self()),
    ememcached_bench_stats:init(),

    TimerPid = ememcached_bench_timekeeper:start_link(),
    WorkerPids = [ememcached_bench_worker:start_link(Address, Port, N) || N <- lists:seq(1, Concurrency)],
    PidList = [TimerPid|WorkerPids],

    ok = proc_lib:init_ack(Parent, {ok, self()}),

    io:format("~nReady?~n"),
    message_loop(PidList).

%% ===================================================================
%% Internal
%% ===================================================================
message_loop(PidList) ->
    receive
        go ->
            io:format("Go~~~n"),
            send(PidList, go),
            message_loop(PidList);
        goal ->
            io:format("Goooooooool! ~n"),
            exit(normal)
    end.

send(PidList, Message) ->
    lists:foreach(fun(Pid) -> Pid ! Message end, PidList).
