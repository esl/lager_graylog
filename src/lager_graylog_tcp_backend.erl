-module(lager_graylog_tcp_backend).

-behaviour(gen_event).

-export([init/1]).
-export([handle_call/2]).
-export([handle_event/2]).
-export([handle_info/2]).
-export([terminate/2]).
-export([code_change/3]).

-define(BACKOFF_START, 1).
-define(BACKOFF_MAX, 30).

-type name() :: {?MODULE, {lager_graylog:host(), lager_graylog:port_number()}}.
-type socket() :: disconnected | {connected, gen_tcp:socket()}.
-type state() :: #{name          := name(),
                   level         := lager_graylog_utils:mask(),
                   host          := lager_graylog:host(),
                   port          := lager_graylog:port_number(),
                   socket        := socket(),
                   backoff       := backoff:backoff(),
                   formatter     := module(),
                   formatter_config := any()}.

%% gen_event callbacks

-spec init([lager_graylog:backend_option()]) -> {ok, state()} | {error, {invalid_opts, term()}}.
init(Opts) ->
    case lager_graylog_utils:parse_common_opts(Opts) of
        {ok, Config} ->
            #{level := Mask,
              host := Host,
              port := Port,
              formatter := Formatter,
              formatter_config := FormatterConfig} = Config,
            State = #{name => {?MODULE, {Host, Port}},
                      level => Mask,
                      host => Host,
                      port => Port,
                      socket => disconnected,
                      backoff => backoff:type(backoff:init(?BACKOFF_START, ?BACKOFF_MAX), jitter),
                      formatter => Formatter,
                      formatter_config => FormatterConfig},
            {ok, try_connect(State)};
        {error, Reason} ->
            {error, {invalid_opts, Reason}}
    end.

handle_call({set_loglevel, Level}, State) ->
    case lager_graylog_utils:validate_loglevel(Level) of
        error ->
            {ok, {error, bad_loglevel}, State};
        {ok, Mask} ->
            {ok, ok, State#{level => Mask}}
    end;
handle_call(get_loglevel, #{level := Level} = State) ->
    {ok, Level, State};
handle_call(_Request, State) ->
    {ok, ok, State}.

handle_event({log, Message}, #{name := Name,
                               level := Mask,
                               socket := {connected, Socket},
                               formatter := Formatter,
                               formatter_config := FormatterConfig} = State) ->
    case lager_util:is_loggable(Message, Mask, Name) of
        true ->
            FormattedLog = Formatter:format(Message, FormatterConfig),
            case gen_tcp:send(Socket, [FormattedLog, 0]) of
                ok ->
                    {ok, State};
                {error, Reason} ->
                    lager:error("Couldn't send log payload: ~p", [Reason]),
                    {ok, try_connect(State#{socket := disconnected})}
            end;
        false ->
            {ok, State}
    end;
handle_event({log, _}, #{socket := disconnected} = State) ->
    {ok, State};
handle_event(_, State) ->
    {ok, State}.

handle_info({tcp_closed, _Socket}, #{socket := {connected, _Socket}} = State) ->
    lager:error("Connection closed", []),
    {ok, try_connect(State#{socket := disconnected})};
handle_info({timeout, _, reconnect}, #{socket := disconnected} = State) ->
    {ok, try_connect(State)};
handle_info(_, State) ->
    {ok, State}.

terminate(_Arg, _State) ->
    ok.

code_change(_OldVsn, State, _Extra) ->
    {ok, State}.

%% Helpers

-spec try_connect(state()) -> state().
try_connect(#{socket := disconnected, host := Host, port := Port, backoff := Backoff} = State) ->
    case gen_tcp:connect(Host, Port, [binary, {active, false}], 5000) of
        {ok, Socket} ->
            {_, NewBackoff} = backoff:succeed(Backoff),
            lager:notice("Connected to ~p:~p~n", [Host, Port]),
            State#{backoff := NewBackoff, socket := {connected, Socket}};
        {error, Reason} ->
            {ReconnectIn, NewBackoff} = backoff:fail(Backoff),
            set_reconnection_timer(ReconnectIn),
            lager:error("Could not connect to ~p:~p: ~p. Retrying in ~ps~n",
                     [Host, Port, Reason, ReconnectIn]),
            State#{backoff := NewBackoff}
    end.

-spec set_reconnection_timer(non_neg_integer()) -> ok.
set_reconnection_timer(ReconnectInSeconds) ->
    erlang:start_timer(ReconnectInSeconds * 1000, self(), reconnect),
    ok.

