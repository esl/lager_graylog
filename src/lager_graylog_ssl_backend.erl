%% This backend was done by modification of 'lager_graylog_tcp_backend', and seems these two could be merged into one.
-module(lager_graylog_ssl_backend).

-behaviour(gen_event).

-export([init/1]).
-export([handle_call/2]).
-export([handle_event/2]).
-export([handle_info/2]).
-export([terminate/2]).
-export([code_change/3]).

-define(BACKOFF_START, 1).
-define(BACKOFF_MAX, 30).

%% It is better to represent internal state as a record:
%%  1. it is more type safe (most problems discovered at compile time)
%%  2. just faster access than maps
-record(state, {
    name, level, host, port, extra_connect_opts, socket,
    backoff, formatter, formatter_config, transport_opts}).

-type name() :: {?MODULE, {lager_graylog:host(), lager_graylog:port_number()}}.
-type socket() :: disconnected | {connected, gen_tcp:socket()}.
-type extra_connect_opts() :: [inet:address_family()].
-type state() :: #{name               := name(),
level              := lager_graylog_utils:mask(),
host               := lager_graylog:host(),
port               := lager_graylog:port_number(),
extra_connect_opts := extra_connect_opts(),
socket             := socket(),
backoff            := backoff:backoff(),
formatter          := module(),
formatter_config   := any()}.

%% gen_event callbacks

-spec init([lager_graylog:backend_option()]) -> {ok, state()} | {error, {invalid_opts, term()}}.
init(Opts) ->
    {ok, _} = application:ensure_all_started(ssl),
    case lager_graylog_utils:parse_common_opts(Opts) of
        {ok, Config} ->
            #{level := Mask,
                host := Host,
                port := Port,
                address_family := AddressFamily,
                formatter := Formatter,
                formatter_config := FormatterConfig,
                transport_opts := TOpts} = Config,
            State = #state{name = {?MODULE, {Host, Port}},
                level = Mask,
                host = Host,
                port = Port,
                extra_connect_opts = extra_connect_opts(AddressFamily),
                socket = disconnected,
                backoff = backoff:type(backoff:init(?BACKOFF_START, ?BACKOFF_MAX), jitter),
                formatter = Formatter,
                formatter_config = FormatterConfig,
                transport_opts = TOpts
            },
            {ok, try_connect(State)};
        {error, Reason} ->
            {error, {invalid_opts, Reason}}
    end.

handle_call({set_loglevel, Level}, State) ->
    case lager_graylog_utils:validate_loglevel(Level) of
        error ->
            {ok, {error, bad_loglevel}, State};
        {ok, Mask} ->
            {ok, ok, State#state{level = Mask}}
    end;
handle_call(get_loglevel, #state{level = Level} = State) ->
    {ok, Level, State};
handle_call(_Request, State) ->
    {ok, ok, State}.

handle_event({log, Message},
    #state{name = Name,
        level = Mask,
        socket = {connected, Socket},
        formatter = Formatter,
        formatter_config = FormatterConfig} = State) ->
    case lager_util:is_loggable(Message, Mask, Name) of
        true ->
            FormattedLog = Formatter:format(Message, FormatterConfig),
            case ssl:send(Socket, [FormattedLog, 0]) of
                ok ->
                    {ok, State};
                {error, Reason} ->
                    lager:error("Couldn't send log payload: ~p", [Reason]),
                    {ok, try_connect(State#state{socket = disconnected})}
            end;
        false ->
            {ok, State}
    end;
handle_event({log, _}, #state{socket = disconnected} = State) ->
    {ok, State};
handle_event(_, State) ->
    {ok, State}.

handle_info({ssl_closed, _Socket}, #state{socket = {connected, _Socket}} = State) ->
    lager:error("Connection closed", []),
    {ok, try_connect(State#state{socket = disconnected})};
handle_info({timeout, _, reconnect}, #state{socket = disconnected} = State) ->
    {ok, try_connect(State)};
handle_info(_, State) ->
    {ok, State}.

terminate(_Arg, _State) ->
    ok.

code_change(_OldVsn, State, _Extra) ->
    {ok, State}.

%% Helpers

-spec try_connect(state()) -> state().
try_connect(#state{socket = disconnected,
    host = Host,
    port = Port,
    extra_connect_opts = ExtraConnectOpts,
    backoff = Backoff,
    transport_opts = TOpts} = State) ->
    case ssl:connect(Host, Port, [binary, {active, false} | ExtraConnectOpts] ++ TOpts, 5000) of
        {ok, Socket} ->
            {_, NewBackoff} = backoff:succeed(Backoff),
            lager:notice("Connected to ~p:~p~n", [Host, Port]),
            State#state{backoff = NewBackoff, socket = {connected, Socket}};
        {error, Reason} ->
            {ReconnectIn, NewBackoff} = backoff:fail(Backoff),
            set_reconnection_timer(ReconnectIn),
            lager:error("Could not connect to ~p:~p: ~p. Retrying in ~ps~n",
                [Host, Port, Reason, ReconnectIn]),
            State#state{backoff = NewBackoff}
    end.

-spec set_reconnection_timer(non_neg_integer()) -> ok.
set_reconnection_timer(ReconnectInSeconds) ->
    erlang:start_timer(ReconnectInSeconds * 1000, self(), reconnect),
    ok.

-spec extra_connect_opts(lager_graylog:address_family()) -> extra_connect_opts().
extra_connect_opts(undefined) -> [];
extra_connect_opts(inet) -> [inet];
extra_connect_opts(inet6) -> [inet6].
