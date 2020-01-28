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
-type socket() :: disconnected | {connected, gen_tcp:socket() | ssl:socket()}.
-type extra_connect_opts() :: [inet:address_family() | lager_graylog:extra_connect_opts()].
-type state() :: #{name               := name(),
                   level              := lager_graylog_utils:mask(),
                   host               := lager_graylog:host(),
                   port               := lager_graylog:port_number(),
                   extra_connect_opts := extra_connect_opts(),
                   socket             := socket(),
                   backoff            := backoff:backoff(),
                   formatter          := module(),
                   formatter_config   := any(),
                   formatter_state    := any(),
                   transport          := lager_graylog:transport()
                  }.

%% gen_event callbacks

-spec init([lager_graylog:backend_option()]) -> {ok, state()} | {error, {invalid_opts, term()}}.
init(Opts) ->
    case lager_graylog_utils:parse_common_opts(Opts) of
        {ok, Config} ->
            #{level := Mask,
              host := Host,
              port := Port,
              address_family := AddressFamily,
              formatter := Formatter,
              formatter_config := FormatterConfig,
              transport := Transport,
              extra_connect_opts := ExtraConnectOpts} = Config,
            State = #{name => {?MODULE, {Host, Port}},
                      level => Mask,
                      host => Host,
                      port => Port,
                      transport => Transport,
                      extra_connect_opts => extra_connect_opts(AddressFamily, ExtraConnectOpts),
                      socket => disconnected,
                      backoff => backoff:type(backoff:init(?BACKOFF_START, ?BACKOFF_MAX), jitter),
                      formatter => Formatter,
                      formatter_config => FormatterConfig,
                      formatter_state => Formatter:init(FormatterConfig)
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
            {ok, ok, State#{level => Mask}}
    end;
handle_call(get_loglevel, #{level := Level} = State) ->
    {ok, Level, State};
handle_call(_Request, State) ->
    {ok, ok, State}.

handle_event({log, Message}, #{name := Name,
                               level := Mask,
                               transport := Transport,
                               socket := {connected, Socket},
                               formatter := Formatter,
                               formatter_config := FormatterConfig,
                               formatter_state := FormatterState
                              } = State) ->
    case lager_util:is_loggable(Message, Mask, Name) of
        true ->
            FormattedLog = Formatter:format(Message, FormatterState, FormatterConfig),
            case Transport:send(Socket, [FormattedLog, 0]) of
                ok ->
                    {ok, State};
                {error, Reason} ->
                    lager:error("Couldn't send log payload due to: ~p", [Reason]),
                    {ok, try_connect(State#{socket := disconnected})}
            end;
        false ->
            {ok, State}
    end;
handle_event({log, _}, #{socket := disconnected} = State) ->
    {ok, State};
handle_event(_, State) ->
    {ok, State}.

handle_info({Closed, _Socket}, #{socket := {connected, _Socket}} = State)
  when Closed == tcp_closed; Closed == ssl_closed ->
    lager:error("Connection closed", []),
    {ok, try_connect(State#{socket := disconnected})};
handle_info({Error, Reason, _Socket}, #{socket := {connected, _Socket}} = State)
  when Error == tcp_error; Error == ssl_error ->
    lager:error("Connection error: ~p", [Reason]),
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
try_connect(#{socket := disconnected,
              host := Host,
              port := Port,
              transport := Transport,
              extra_connect_opts := ExtraConnectOpts,
              backoff := Backoff} = State) ->
    case Transport:connect(Host, Port, [binary, {active, false} | ExtraConnectOpts], 5000) of
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

-spec extra_connect_opts(lager_graylog:address_family(), lager_graylog:extra_connect_opts()) -> extra_connect_opts().
extra_connect_opts(undefined, ExtraConnectOpts) ->
    [{keepalive, true},
     {send_timeout, proplists:get_value(send_timeout, ExtraConnectOpts, 5000)},
     {send_timeout_close, true} |
     proplists:delete(send_timeout, ExtraConnectOpts)];
extra_connect_opts(inet, ExtraConnectOpts) ->
    [inet | extra_connect_opts(undefined, ExtraConnectOpts)];
extra_connect_opts(inet6, ExtraConnectOpts) ->
    [inet6 | extra_connect_opts(undefined, ExtraConnectOpts)].
