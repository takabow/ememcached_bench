-module(ememcached_client_api).

%% API
-export([test/2, test/1]).

%% API
-export([connect/2, get/2, set/3, delete/2, incr/3, decr/3, quit/1]).

%% ===================================================================
%% API
%% ===================================================================
test(Address, Port) ->
    io:format("Connect to ~p:~p~n",[Address, Port]),
    {ok, CSock} = connect(Address, Port),
    test(CSock).

test(CSock) ->
    io:format("test start!~n"),

    %% set/get test
    ok = set(CSock, "key1", "abcdef"),
    {ok, {"key1", _, 6, "abcdef"}} = get(CSock, "key1"),
    ok = set(CSock, "key2", "5"),
    {ok, {"key2", _, 1, "5"}} = get(CSock, "key2"),
    ok = set(CSock, "key3", "ab\r\ncd\r\n"),
    {ok, {"key3", _, 8, "ab\r\ncd\r\n"}} = get(CSock, "key3"),
    ok = set(CSock, "key4", "012"),
    {ok, {"key4", _, 3, "012"}} = get(CSock, "key4"),
    io:format("[set/get] pass.~n"),

    %% incr test
    ok = set(CSock, "key", "5"),
    {ok, {"key", _, 1, "5"}} = get(CSock, "key"),
    {ok, 20} = incr(CSock, "key", 15),
    {ok, {"key", _, 2, "20"}} = get(CSock, "key"),
    {ok, 1520} = incr(CSock, "key", 1500),
    {ok, {"key", _, 4, "1520"}} = get(CSock, "key"),
    io:format("[incr] pass.~n"),

    %% decr test
    ok = set(CSock, "key", "18"),
    {ok, {"key", _, 2, "18"}} = get(CSock, "key"),
    {ok, 8} = decr(CSock, "key", 10),
    {ok, {"key", _, 1, "8"}} = get(CSock, "key"),
    {ok, 0} = decr(CSock, "key", 9),
    {ok, {"key", _, 1, "0"}} = get(CSock, "key"),
    io:format("[decr] pass.~n"),

    %% delete test
    ok = set(CSock, "key", "test data for delete."),
    ok = delete(CSock, "key"),
    notfound = delete(CSock, "key"),
    io:format("[delete] pass.~n"),

    io:format("done.~n"),

    ok = quit(CSock).

connect(Address, Port) ->
    {ok, _} = gen_tcp:connect(Address, Port, [binary, {packet, 0}, {active, false}]).

get(ConnectionSocket, Key) ->
    ok = gen_tcp:send(ConnectionSocket, ["get ", Key, "\r\n"]),
    case gen_tcp:recv(ConnectionSocket, 0) of
        {ok, <<"END\r\n">>} ->
            notfound;
        {ok, <<"ERROR\r\n">>} ->
            error;
        {ok, Data} ->
            [Header, MaybeValue] = binary:split(Data, [<<"\r\n">>], []),
            [<<"VALUE">>, BinKey, BinFlags, BinBytes|_] = binary:split(Header, [<<" ">>], [global]),
            Bytes = binary_to_integer(BinBytes),
            <<Value:Bytes/binary, "\r\nEND\r\n">> = MaybeValue,
            {ok, {binary_to_list(BinKey), binary_to_list(BinFlags), Bytes, binary_to_list(Value)}};
        {error, Reason} ->
            io:format("Error: ~p~n", [Reason])
    end.

set(ConnectionSocket, Key, Value) ->
    Bytes = length(Value),
    ok = gen_tcp:send(ConnectionSocket, ["set ", Key, " 0 0 ", integer_to_list(Bytes), "\r\n"]),
    ok = gen_tcp:send(ConnectionSocket, [Value, "\r\n"]),

    case gen_tcp:recv(ConnectionSocket, 0) of
        {ok, <<"STORED\r\n">>} ->
            ok;
        {ok, <<"ERROR\r\n">>} ->
            error;
        {error, Reason} ->
            io:format("Error: ~p~n", [Reason]),
            error
    end.

delete(ConnectionSocket, Key) ->
    ok = gen_tcp:send(ConnectionSocket, ["delete ", Key, "\r\n"]),
    case gen_tcp:recv(ConnectionSocket, 0) of
        {ok, <<"NOT_FOUND\r\n">>} ->
            notfound;
        {ok, <<"ERROR\r\n">>} ->
            error;
        {ok, <<"DELETED\r\n">>} ->
            ok;
        {error, Reason} ->
            io:format("Error: ~p~n", [Reason]),
            error
    end.

incr(ConnectionSocket, Key, Value) when is_integer(Value) ->
    ok = gen_tcp:send(ConnectionSocket, ["incr ", Key, " ", integer_to_list(Value), "\r\n"]),
    case gen_tcp:recv(ConnectionSocket, 0) of
        {ok, <<"NOT_FOUND\r\n">>} ->
            notfound;
        {ok, <<"ERROR\r\n">>} ->
            error;
        {ok, Data} ->
            [BinValue|_] = binary:split(Data, [<<"\r\n">>], [global]),
            {ok, binary_to_integer(BinValue)};
        {error, Reason} ->
            io:format("Error: ~p~n", [Reason]),
            error
    end.

decr(ConnectionSocket, Key, Value) when is_integer(Value) ->
    ok = gen_tcp:send(ConnectionSocket, ["decr ", Key, " ", integer_to_list(Value), "\r\n"]),
    case gen_tcp:recv(ConnectionSocket, 0) of
        {ok, <<"NOT_FOUND\r\n">>} ->
            notfound;
        {ok, <<"ERROR\r\n">>} ->
            error;
        {ok, Data} ->
            [BinValue|_] = binary:split(Data, [<<"\r\n">>], [global]),
            {ok, binary_to_integer(BinValue)};
        {error, Reason} ->
            io:format("Error: ~p~n", [Reason]),
            error
    end.

quit(ConnectionSocket) ->
    ok = gen_tcp:send(ConnectionSocket, ["quit"]),
    {error, Reason} = gen_tcp:recv(ConnectionSocket, 0),
    io:format("quit: ~p~n", [Reason]),
    ok.



