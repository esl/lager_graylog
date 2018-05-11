-module(lager_graylog_tcp_backend_SUITE).

-include_lib("eunit/include/eunit.hrl").
-include_lib("common_test/include/ct.hrl").

-compile(export_all).

-define(HOST, {127, 0, 0, 1}).

%% Suite configuration

all() ->
    [{group, all}].

groups() ->
    [{all, [], test_cases()}].

test_cases() ->
    [sends_log_messages_to_configured_endpoint,
     doesnt_log_over_configured_level,
     drops_log_messages_if_there_is_no_connection_and_reconnects_later
    ].

init_per_suite(Config) ->
    application:set_env(lager, error_logger_redirect, false),
    lager:start(),
    Config.

end_per_suite(_) ->
    application:stop(lager).

%% Test cases

sends_log_messages_to_configured_endpoint(_Config) ->
    {Socket, Port} = listen(),
    start_lager_handler(Port),
    RecvSocket = accept(Socket),
    flush(RecvSocket),

    Log1 = log(info, "info log message"),
    Log2 = log(critical, "critical log message"),

    Logs = flush(RecvSocket),
    assert_logged(Logs, [Log1, Log2]),

    stop_lager_handler(Port).

doesnt_log_over_configured_level(_Config) ->
    {Socket, Port} = listen(),
    start_lager_handler(Port),
    RecvSocket = accept(Socket),
    flush(RecvSocket),

    Log1 = log(info, "log message 1"),
    ok = lager:set_loglevel(handler_id(Port), warning),
    Log2 = log(info, "log message 2"),

    Logs = flush(RecvSocket),
    assert_logged(Logs, [Log1]),
    assert_not_logged(Logs, [Log2]),

    stop_lager_handler(Port).

drops_log_messages_if_there_is_no_connection_and_reconnects_later(_Config) ->
    {Socket, Port} = listen(),
    start_lager_handler(Port),
    RecvSocket1 = accept(Socket),
    flush(RecvSocket1),

    Log1 = log(info, "log message 1"),
    Logs1 = flush(RecvSocket1),
    close(RecvSocket1),
    Log2 = log(info, "log message 2"),
    Log3 = log(info, "log message 3"),

    RecvSocket2 = accept(Socket),
    Log4 = log(info, "log message 4"),
    Logs2 = flush(RecvSocket2),

    Logs = Logs1 ++ Logs2,
    assert_logged(Logs, [Log1, Log4]),
    assert_not_logged(Logs, [Log2, Log3]),

    stop_lager_handler(Port).

%% Helpers

-spec start_lager_handler(inet:port_number()) -> ok.
start_lager_handler(Port) ->
    Opts = [{host, ?HOST}, {port, Port}],
    ok = gen_event:add_handler(lager_event, handler_id(Port), Opts).

-spec stop_lager_handler(inet:port_number()) -> ok.
stop_lager_handler(Port) ->
    ok = gen_event:delete_handler(lager_event, handler_id(Port), []).

-spec listen() -> {gen_tcp:socket(), inet:port_number()}.
listen() ->
    {ok, Socket} = gen_tcp:listen(0, [binary,
                                      {ip, ?HOST},
                                      {active, false},
                                      {reuseaddr, true}]),
    {ok, Port} = inet:port(Socket),
    {Socket, Port}.

-spec accept(gen_tcp:socket()) -> gen_tcp:socket().
accept(Socket) ->
    {ok, RecvSocket} = gen_tcp:accept(Socket, 1000),
    RecvSocket.

-spec close(gen_tcp:socket()) -> ok.
close(RecvSocket) ->
    gen_tcp:close(RecvSocket).

-spec flush(gen_tcp:socket()) -> [map()].
flush(RecvSocket) ->
    Data = iolist_to_binary(recv(RecvSocket)),
    Logs = binary:split(Data, <<0>>, [trim_all, global]),
    [jsx:decode(Log, [return_maps]) || Log <- Logs].

-spec recv(gen_tcp:socket()) -> binary().
recv(RecvSocket) ->
    recv(RecvSocket, <<>>).

-spec recv(gen_tcp:socket(), iodata()) -> iodata().
recv(RecvSocket, Acc) ->
    case gen_tcp:recv(RecvSocket, 0, 100) of
        {ok, Data} ->
            ct:pal("DATA: ~p", [Data]),
            recv(RecvSocket, [Acc, Data]);
        {error, timeout} ->
            Acc
    end.

-spec handler_id(inet:port_number()) -> term().
handler_id(Port) ->
    {lager_graylog_tcp_backend, {?HOST, Port}}.


-spec log(atom(), string()) -> pos_integer().
log(Level, Message) ->
    LogRef = erlang:unique_integer([positive, monotonic]),
    lager:log(Level, [{test_log_ref, LogRef}], Message),
    LogRef.

-spec assert_logged([map()], [pos_integer()]) -> ok | no_return().
assert_logged(_Logs, []) ->
    ok;
assert_logged([#{<<"_test_log_ref">> := LogRef} | Logs], [LogRef | LogRefs]) ->
    assert_logged(Logs, LogRefs);
assert_logged([_ | Logs], LogRefs) ->
    assert_logged(Logs, LogRefs);
assert_logged(Logs, [LogRef | _]) ->
    error({log_not_found, [{log_ref, LogRef}, {remaining_logs, Logs}]}).

-spec assert_not_logged([map()], [pos_integer()]) -> ok | no_return().
assert_not_logged(Logs, LogRefs) ->
    FilteredLogs = lists:filter(
                       fun(#{<<"_test_log_ref">> := LogRef}) -> lists:member(LogRef, LogRefs);
                          (_) -> false %% filter out logs which weren't produced by a test case
                       end, Logs),
    case FilteredLogs of
        [] ->
            ok;
        _ ->
            error({found_logs_but_shouldnt, [{log_refs, LogRefs}, {found_logs, FilteredLogs}]})
    end.

