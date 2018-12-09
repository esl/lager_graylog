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
     doesnt_log_over_configured_level,
     big_message_should_be_chunked,
     too_big_message_should_be_dropped,
     chunks_must_fit_in_specified_size
    ].

init_per_suite(Config) ->
    application:set_env(lager, error_logger_redirect, false),
    lager:start(),
    Config.

end_per_suite(_) ->
    application:stop(lager).

init_per_testcase(_, Config) ->
    {Socket, Port} = open(),
    AdditionalOptions = [{chunk_size, 1372}],
    start_lager_handler(Port, AdditionalOptions),
    AdditionalOptions ++ [{socket, Socket}, {port, Port} | Config].

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

big_message_should_be_chunked(Config) ->
    Socket = ?config(socket, Config),
    ChunkSize = ?config(chunk_size, Config),
    BigMessage = generate_message(ChunkSize * 15 + 100),
    Log1 = log(info, BigMessage),
    assert_logged([flush_chunked(Socket, 16)], [Log1]).

too_big_message_should_be_dropped(Config) ->
    Socket = ?config(socket, Config),
    ChunkSize = ?config(chunk_size, Config),
    VeryBigMessage = generate_message(ChunkSize * 129),
    log(info, VeryBigMessage),
    [#{<<"level">> := 3, <<"_module">> := <<"lager_graylog_udp_backend">>}] = flush(Socket).

chunks_must_fit_in_specified_size(Config) ->
    Socket = ?config(socket, Config),
    ChunkSize = ?config(chunk_size, Config),
    Message = generate_message(ChunkSize * 5 + 100),
    log(info, Message),
    Chunks = recv(Socket, 129, 50, []),
    %% All chunks but last should be fully loaded
    [_LastChunk] =
        lists:filter(
          fun
              (Chunk) when byte_size(Chunk) =:= ChunkSize -> false;
              (Chunk) when byte_size(Chunk) < ChunkSize   -> true
          end, Chunks).

%% Helpers

generate_message(Length) ->
    iolist_to_binary([integer_to_list(crypto:rand_uniform(0, 36), 36) ||
                         _ <- lists:seq(1, Length)]).

-spec start_lager_handler(inet:port_number(), [lager_graylog:udp_backend_option()]) -> ok.
start_lager_handler(Port, AdditionalOptions) ->
    Opts = [{host, ?HOST}, {port, Port} | AdditionalOptions],
    ok = gen_event:add_handler(lager_event, handler_id(Port), Opts).

-spec stop_lager_handler(inet:port_number()) -> ok.
stop_lager_handler(Port) ->
    ok = gen_event:delete_handler(lager_event, handler_id(Port), []).

-spec handler_id(inet:port_number()) -> term().
handler_id(Port) ->
    {lager_graylog_udp_backend, {?HOST, Port}}.

-spec open() -> {gen_udp:socket(), inet:port_number()}.
open() ->
    {ok, Socket} = gen_udp:open(0, [
                                    binary,
                                    {ip, ?HOST},
                                    {active, true},
                                    {reuseaddr, true},
                                    {recbuf, 191100}
                                   ]),
    {ok, Port} = inet:port(Socket),
    {Socket, Port}.

flush_chunked(Socket, NumberOfChunks) ->
    Chunks = recv(Socket, 129, 500, []),
    DecodedChunks =
        lists:map(
          fun(<<30,15, MessageId:8/binary, SequenceNumber:8/integer,
                ChunksTotal:8/integer, BodyPart/binary>>) ->
                  {{MessageId, ChunksTotal}, SequenceNumber , BodyPart}
          end, Chunks),
    {MessageIdsAndChunkTotals, SequenceNumbers, BodyParts} = lists:unzip3(DecodedChunks),
    %% all MessageIds and ChunkTotals shold be the same for one message
    1 = length(lists:usort(MessageIdsAndChunkTotals)),
    %% we should receive all chunks (possible reordered)
    true = lists:seq(0, NumberOfChunks - 1) =:= lists:sort(SequenceNumbers),
    NumberOfChunks = length(Chunks),
    jiffy:decode(BodyParts, [return_maps]).

-spec flush(gen_udp:socket()) -> [map()].
flush(Socket) ->
    Packets = recv(Socket),
    [jiffy:decode(Packet, [return_maps]) || Packet <- Packets].

-spec recv(gen_udp:socket()) -> ok.
recv(Socket) ->
    recv(Socket, 10, 50, []).

-spec recv(gen_udp:socket(), Tries :: non_neg_integer(), timeout(), list()) -> [binary()].
recv(Socket, Tries, Timeout, Acc0) when Tries > 0 ->
    receive
        {udp, Socket, _, _, Packet} ->
            recv(Socket, Tries -  1, Timeout, [Acc0, Packet])
    after
        Timeout -> lists:flatten(Acc0)
    end;
recv(_Socket, 0, _Timeout, Acc) -> lists:flatten(Acc).

-spec log(atom(), string()) -> pos_integer().
log(Level, Message) ->
    LogRef = erlang:unique_integer([positive, monotonic]),
    lager:log_unsafe(Level, [{test_log_ref, LogRef}], Message, []),
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
