-module(ememcached_bench_worker).

%% API
-export([start_link/3, init/3]).

%% ===================================================================
%% API
%% ===================================================================
start_link(Address, Port, N) ->
    spawn_link(?MODULE, init, [Address, Port, N]).

%% ===================================================================
%% Callback
%% ===================================================================
init(Address, Port, _) ->
    {ok, ConnectionSocket} = ememcached_client_api:connect(Address, Port),
    ready(ConnectionSocket).

%% ===================================================================
%% Internal
%% ===================================================================
ready(ConnectionSocket) ->
    UniqString = io_lib:format("~p",[self()]),    
    ememcached_bench_stats:init_counter(UniqString, s1),
    receive
        go ->
            run(ConnectionSocket, UniqString)
    end.

run(ConnectionSocket, UniqString) ->
    %StartTime = ememcached_bench_stats:start(),
    %ememcached_bench:get_bench(ConnectionSocket,10000, 10),
    ememcached_bench:scenario1(ConnectionSocket, UniqString, 10000),
    %ememcached_bench_stats:stop(StartTime, 100),
    ememcached_bench_stats:update_counter(UniqString, 1),
    run(ConnectionSocket, UniqString).
