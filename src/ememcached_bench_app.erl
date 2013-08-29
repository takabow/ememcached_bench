-module(ememcached_bench_app).

-behaviour(application).

-export([start/2, stop/1]).

%% ===================================================================
%% Application callbacks
%% ===================================================================
start(_StartType, _StartArgs) ->
    {ok, Address} = application:get_env(ememcached_bench, address),
    {ok, Port} = application:get_env(ememcached_bench, port),
    {ok, Concurrency} = application:get_env(ememcached_bench, concurrency),
    ememcached_bench_sup:start_link(Address, Port, Concurrency).

stop(_State) ->
    ok.
