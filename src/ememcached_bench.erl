-module(ememcached_bench).

%% API
-export([start/0, go/0, goal/0]).

% API - scenario
-export([scenario1/3]).

%% ===================================================================
%% API
%% ===================================================================
start() ->
    application:start(ememcached_bench).

go() ->
    Pid = whereis(ememcached_bench_sup),
    Pid ! go.

goal() ->
    Pid = whereis(ememcached_bench_sup),
    Pid ! goal.

%% ===================================================================
%% API
%% ===================================================================
scenario1(CSock, UniqString, MaxKeyNum) ->
    N = random:uniform(MaxKeyNum),
    NString = integer_to_list(N),
    Key = "key" ++ UniqString ++ NString,
    Value = "value" ++ NString,

    ok = ememcached_client_api:set(CSock, Key, Value),
    {ok, _} = ememcached_client_api:get(CSock, Key),

    ok = ememcached_client_api:set(CSock, Key, NString),
    {ok, _} = ememcached_client_api:incr(CSock, Key, N),
    {ok, _} = ememcached_client_api:get(CSock, Key),
    {ok, _} = ememcached_client_api:decr(CSock, Key, N),
    {ok, _} = ememcached_client_api:get(CSock, Key),

    ok = ememcached_client_api:delete(CSock, Key).



