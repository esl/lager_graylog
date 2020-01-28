-module(lager_graylog_tcp_backend_SUITE).

-include_lib("eunit/include/eunit.hrl").
-include_lib("common_test/include/ct.hrl").

-compile(export_all).

-define(HOST, {127, 0, 0, 1}).

-ifdef(OTP_RELEASE).
-define(SSL_HANDSHAKE(TransportAccept), ssl:handshake(TransportAccept)).
-else.
-define(SSL_HANDSHAKE(TransportAccept),
        begin
            TransportSocket = TransportAccept,
            ok = ssl:ssl_accept(TransportSocket),
            {ok, TransportSocket}
        end).
-endif.


-type socket() :: gen_tcp:socket() | ssl:socket().

%% Suite configuration

all() ->
    [{group, gen_tcp},
     {group, ssl}].

groups() ->
    [{gen_tcp, [], test_cases()},
     {ssl, [], test_cases()}].

test_cases() ->
    [sends_log_messages_to_configured_endpoint,
     doesnt_log_over_configured_level,
     drops_log_messages_if_there_is_no_connection_and_reconnects_later
    ].

init_per_suite(Config) ->
    application:set_env(lager, error_logger_redirect, false),
    {ok, Started} = application:ensure_all_started(ssl),
    lager:start(),
    [{started, [lager | Started]} | Config].

end_per_suite(Config) ->
    StartedApps = ?config(started, Config),
    lists:foreach(fun application:stop/1, lists:reverse(StartedApps)).

init_per_group(Transport, Config) ->
    [{transport, Transport} | Config].

end_per_group(_Transport, _Config) ->
    ok.

init_per_testcase(_, Config) ->
    Transport = ?config(transport, Config),
    {Socket, Port} = listen(Transport),
    start_lager_handler(Transport, Port),
    RecvSocket = accept(Transport, Socket),
    flush(Transport, RecvSocket),
    [{socket, Socket}, {recv_socket, RecvSocket}, {port, Port} | Config].

end_per_testcase(_, Config) ->
    Transport = ?config(transport, Config),
    close(Transport, ?config(recv_socket, Config)),
    close(Transport, ?config(socket, Config)),
    stop_lager_handler(?config(port, Config)).

%% Test cases

sends_log_messages_to_configured_endpoint(Config) ->
    Log1 = log(info, "info log message"),
    Log2 = log(critical, "critical log message"),

    Logs = flush(?config(transport, Config), ?config(recv_socket, Config)),
    assert_logged(Logs, [Log1, Log2]).

doesnt_log_over_configured_level(Config) ->
    Log1 = log(info, "log message 1"),
    ok = lager:set_loglevel(handler_id(?config(port, Config)), warning),
    Log2 = log(info, "log message 2"),

    Logs = flush(?config(transport, Config), ?config(recv_socket, Config)),
    assert_logged(Logs, [Log1]),
    assert_not_logged(Logs, [Log2]).

drops_log_messages_if_there_is_no_connection_and_reconnects_later(Config) ->
    Transport = ?config(transport, Config),
    Port = ?config(port, Config),
    RecvSocket1 = ?config(recv_socket, Config),

    Log1 = log(info, "log message 1"),
    Logs1 = flush(?config(transport, Config), RecvSocket1),
    assert_logged(Logs1, [Log1]),

    close(Transport, RecvSocket1),
    % When the connection-closed, the backend tries to create a new connection.
    % With TCP the new connection is pending on the server 'accept'-ing the connection.
    % However with SSL, the connection seem to 'accepted' by the SSL stack.
    %
    % This difference leads to inconsistent test behaviour,
    % where 'Log2' and 'Log3', being in the message-box of the backend,
    % maybe logged once the new SSL connection is up.
    %
    % The log messages, on connection-close, from within backend add to the
    % complexity of testing the reconnection behaviour.
    %
    % To get a consistent behaviour, the server has to be recreated here:
    close(Transport, ?config(socket, Config)),
    timer:sleep(100),
    {ListenSocket, Port} = listen(Transport, Port),

    Log2 = log(info, "log message 2"),
    Log3 = log(info, "log message 3"),

    RecvSocket2 = accept(Transport, ListenSocket),
    timer:sleep(500),

    Log4 = log(info, "log message 4"),
    Logs2 = flush(Transport, RecvSocket2),

    Logs = Logs1 ++ Logs2,
    assert_logged(Logs, [Log1, Log4]),
    assert_not_logged(Logs, [Log2, Log3]),

    close(Transport, ListenSocket).

%% Helpers

-spec start_lager_handler(lager_graylog:transport(), inet:port_number()) -> ok.
start_lager_handler(Transport, Port) ->
    Opts = [{host, ?HOST}, {port, Port},
            {transport, Transport},
            {extra_connect_opts, mk_opts(Transport, connect)}],
    spawn(gen_event, add_handler, [lager_event, handler_id(Port), Opts]).

-spec stop_lager_handler(inet:port_number()) -> ok.
stop_lager_handler(Port) ->
    ok = gen_event:delete_handler(lager_event, handler_id(Port), []).

-spec listen(lager_graylog:transport()) -> {socket(), inet:port_number()}.
listen(Transport) ->
    listen(Transport, 0).

listen(Transport, PortNo) ->
    {ok, Socket} = Transport:listen(PortNo, [binary,
                                           {ip, ?HOST},
                                           {active, false},
                                           {reuseaddr, true}
                                           |mk_opts(Transport, listen)]),
    SocketModule = case Transport of
        gen_tcp -> inet;
        _ -> Transport
    end,
    {ok, {_Addr, Port}} = SocketModule:sockname(Socket),
    {Socket, Port}.

-spec accept(lager_graylog:transport(), socket()) -> socket().
accept(gen_tcp, Socket) ->
    accept(gen_tcp, accept, Socket);
accept(ssl, Socket) ->
    {ok, TlsSocket} = ?SSL_HANDSHAKE(accept(ssl, transport_accept, Socket)),
    TlsSocket.

-spec accept(lager_graylog:transport(), accept | transport_accept, socket()) -> socket().
accept(Transport, Accept, Socket) ->
    {ok, RecvSocket} = Transport:Accept(Socket, 5000),
    RecvSocket.

-spec close(lager_graylog:transport(), socket()) -> ok.
close(Transport, RecvSocket) ->
    Transport:close(RecvSocket).

-spec flush(lager_graylog:transport(), socket()) -> [map()].
flush(Transport, RecvSocket) ->
    Data = iolist_to_binary(recv(Transport, RecvSocket)),
    Logs = binary:split(Data, <<0>>, [trim_all, global]),
    [jiffy:decode(Log, [return_maps]) || Log <- Logs].

-spec recv(lager_graylog:transport(), socket()) -> binary().
recv(Transport, RecvSocket) ->
    recv(Transport, RecvSocket, <<>>).

-spec recv(lager_graylog:transport(), socket(), iodata()) -> iodata().
recv(Transport, RecvSocket, Acc) ->
    case Transport:recv(RecvSocket, 0, 100) of
        {ok, Data} ->
            ct:pal("DATA: ~p", [Data]),
            recv(Transport, RecvSocket, [Acc, Data]);
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

mk_opts(gen_tcp, _) ->
    [];
mk_opts(ssl, listen) ->
    mk_opts("server");
mk_opts(ssl, connect) ->
    mk_opts("client").

mk_opts(Role) ->
    Dir = filename:join([code:lib_dir(ssl), "examples", "certs", "etc"]),
    [{verify, 2},
     {depth, 2},
     {server_name_indication, disable},
     {cacertfile, filename:join([Dir, Role, "cacerts.pem"])},
     {certfile, filename:join([Dir, Role, "cert.pem"])},
     {keyfile, filename:join([Dir, Role, "key.pem"])}].
