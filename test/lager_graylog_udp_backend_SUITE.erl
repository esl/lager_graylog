-module(lager_graylog_udp_backend_SUITE).

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
     doesnt_log_over_configured_level
    ].

init_per_suite(Config) ->
    application:set_env(lager, error_logger_redirect, false),
    lager:start(),
    Config.

end_per_suite(_) ->
    application:stop(lager).

init_per_testcase(_, Config) ->
    {Socket, Port} = open(),
    start_lager_handler(Port),
    [{socket, Socket}, {port, Port} | Config].

end_per_testcase(_, Config) ->
    stop_lager_handler(?config(port, Config)).

%% Test cases

sends_log_messages_to_configured_endpoint(Config) ->
    Socket = ?config(socket, Config),

    Log1 = log(info, "info log message"),
    Log2 = log(critical, "critical log message"),

    Logs = flush(Socket),
    assert_logged(Logs, [Log1, Log2]).

doesnt_log_over_configured_level(Config) ->
    Socket = ?config(socket, Config),

    Log1 = log(info, "log message 1"),
    ok = lager:set_loglevel(handler_id(?config(port, Config)), warning),
    Log2 = log(info, "log message 2"),

    Logs = flush(Socket),
    assert_logged(Logs, [Log1]),
    assert_not_logged(Logs, [Log2]).

%% Helpers

-spec start_lager_handler(inet:port_number()) -> ok.
start_lager_handler(Port) ->
    Opts = [{host, ?HOST}, {port, Port}],
    ok = gen_event:add_handler(lager_event, handler_id(Port), Opts).

-spec stop_lager_handler(inet:port_number()) -> ok.
stop_lager_handler(Port) ->
    ok = gen_event:delete_handler(lager_event, handler_id(Port), []).

-spec handler_id(inet:port_number()) -> term().
handler_id(Port) ->
    {lager_graylog_udp_backend, {?HOST, Port}}.

-spec open() -> {gen_udp:socket(), inet:port_number()}.
open() ->
    {ok, Socket} = gen_udp:open(0, [binary,
                                    {ip, ?HOST},
                                    {active, true},
                                    {reuseaddr, true}]),
    {ok, Port} = inet:port(Socket),
    {Socket, Port}.

-spec flush(gen_udp:socket()) -> [map()].
flush(Socket) ->
    Packets = recv(Socket),
    [jiffy:decode(Packet, [return_maps]) || Packet <- Packets].

-spec recv(gen_udp:socket()) -> ok.
recv(Socket) ->
    recv(Socket, 10, 10, []).

-spec recv(gen_udp:socket(), Tries :: non_neg_integer(), timeout(), list()) -> [binary()].
recv(Socket, Tries, Timeout, Acc0) when Tries > 0 ->
    Acc =
        receive
            {udp, Socket, _, _, Packet} ->
                [Acc0, Packet]
        after
            Timeout ->
                Acc0
        end,
    recv(Socket, Tries -  1, Timeout, Acc);
recv(_Socket, 0, _Timeout, Acc) ->
    lists:flatten(Acc).

-spec log(atom(), string()) -> pos_integer().
log(Level, Message) ->
    LogRef = erlang:unique_integer([positive, monotonic]),
    lager:log(Level, [{test_log_ref, LogRef}], Message),
    LogRef.

-spec assert_logged([map()], [pos_integer()]) -> ok | no_return().
assert_logged(Logs, ExpectedLogRefs) ->
    LogRefsSet = make_log_refs_set(Logs),
    lists:foreach(fun(LogRef) ->
                      case ordsets:is_element(LogRef, LogRefsSet) of
                          true ->
                              ok;
                          false ->
                              error({log_not_found, [{log_ref, LogRef}, {logs, Logs}]})
                      end
                  end, ExpectedLogRefs).

-spec make_log_refs_set([map()]) -> ordsets:ordset(pos_integer()).
make_log_refs_set(Logs) ->
    LogRefs = [LogRef || #{<<"_test_log_ref">> := LogRef} <- Logs],
    ordsets:from_list(LogRefs).

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

