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

init_per_testcase(_, Config) ->
    {Socket, Port} = listen(),
    start_lager_handler(Port),
    RecvSocket = accept(Socket),
    flush(RecvSocket),
    [{socket, Socket}, {recv_socket, RecvSocket}, {port, Port} | Config].

end_per_testcase(_, Config) ->
    stop_lager_handler(?config(port, Config)).

%% Test cases

sends_log_messages_to_configured_endpoint(Config) ->
    RecvSocket = ?config(recv_socket, Config),

    lager:info("info log message"),
    lager:critical("critical log message"),

    ok = recv(RecvSocket),
    ok = recv(RecvSocket),
    nothing = recv(RecvSocket).

doesnt_log_over_configured_level(Config) ->
    RecvSocket = ?config(recv_socket, Config),

    lager:info("log message"),
    ok = recv(RecvSocket),

    ok = lager:set_loglevel(handler_id(?config(port, Config)), warning),

    lager:info("log message"),
    nothing = recv(RecvSocket).

drops_log_messages_if_there_is_no_connection_and_reconnects_later(Config) ->
    RecvSocket1 = ?config(recv_socket, Config),

    lager:info("log message 1"),
    ok = recv(RecvSocket1),

    close(RecvSocket1),
    lager:info("log message 2"),
    lager:info("log message 3"),

    RecvSocket2 = accept(?config(socket, Config)),
    {ok, #{<<"short_message">> := LogMessage1}} = recv_with_payload(RecvSocket2),
    {ok, #{<<"short_message">> := LogMessage2}} = recv_with_payload(RecvSocket2),
    ?assertMatch({_, _}, binary:match(LogMessage1, <<"Couldn't send log payload">>)),
    ?assertMatch({_, _}, binary:match(LogMessage2, <<"Connected to">>)),
    nothing = recv(RecvSocket2).

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

-spec recv(gen_tcp:socket()) -> ok | nothing.
recv(RecvSocket) ->
    case recv_with_payload(RecvSocket) of
        {ok, _} ->
            ok;
        nothing ->
            nothing
    end.

-spec recv_with_payload(gen_tcp:socket()) -> {ok, map()} | nothing.
recv_with_payload(RecvSocket) ->
    receive
        {log, Log} ->
            {ok, Log}
    after
        0 ->
            maybe_recv_from_socket(RecvSocket)
    end.

-spec maybe_recv_from_socket(gen_tcp:socket()) -> {ok, map()} | nothing.
maybe_recv_from_socket(RecvSocket) ->
    case gen_tcp:recv(RecvSocket, 0, 1000) of
        {ok, Data} ->
            Logs = binary:split(Data, <<0>>, [trim_all, global]),
            [self() ! {log, jsx:decode(Log, [return_maps])} || Log <- Logs],
            recv_with_payload(RecvSocket);
        {error, timeout} ->
            nothing
    end.

-spec flush(gen_tcp:socket()) -> ok.
flush(RecvSocket) ->
    case gen_tcp:recv(RecvSocket, 0, 1000) of
        % only handle successful case or timeout - let the other errors manifest themselves
        {ok, _} ->
            ok;
        {error, timeout} ->
            ok
    end.

-spec handler_id(inet:port_number()) -> term().
handler_id(Port) ->
    {lager_graylog_tcp_backend, {?HOST, Port}}.
