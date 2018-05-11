-module(lager_graylog_udp_backend).

-behaviour(gen_event).

-export([init/1]).
-export([handle_call/2]).
-export([handle_event/2]).
-export([handle_info/2]).
-export([terminate/2]).
-export([code_change/3]).

-type name() :: {?MODULE, {lager_graylog:host(), lager_graylog:port_number()}}.
-type socket() :: gen_udp:socket().
-type state() :: #{name          := name(),
                   level         := lager_graylog_utils:mask(),
                   host          := lager_graylog:host(),
                   port          := lager_graylog:port_number(),
                   socket        := socket(),
                   formatter     := module(),
                   formatter_config := any()}.

%% gen_event callbacks

init(Opts) ->
    case lager_graylog_utils:parse_common_opts(Opts) of
        {ok, Config} ->
            open_socket_and_init_state(Config);
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
                               host := Host,
                               port := Port,
                               socket := Socket,
                               formatter := Formatter,
                               formatter_config := FormatterConfig} = State) ->
    case lager_util:is_loggable(Message, Mask, Name) of
        true ->
            FormattedLog = Formatter:format(Message, FormatterConfig),
            case gen_udp:send(Socket, Host, Port, FormattedLog) of
                ok ->
                    {ok, State};
                {error, Reason} ->
                    lager:error("Couldn't send log payload: ~p", [Reason]),
                    {ok, State}
            end;
        false ->
            {ok, State}
    end.

handle_info(_, State) ->
    {ok, State}.

terminate(_Arg, _State) ->
    ok.

code_change(_OldVsn, State, _Extra) ->
    {ok, State}.

%% Helpers

-spec open_socket_and_init_state(lager_graylog_utils:common_config()) ->
    {ok, state()} | {error, {gen_udp_open_failed, term()}}.
open_socket_and_init_state(Config) ->
    #{level := Mask,
      host := Host,
      port := Port,
      formatter := Formatter,
      formatter_config := FormatterConfig} = Config,
    case gen_udp:open(0, [binary, {active, false}]) of
        {ok, Socket} ->
            State = #{name => {?MODULE, {Host, Port}},
                      level => Mask,
                      host => Host,
                      port => Port,
                      socket => Socket,
                      formatter => Formatter,
                      formatter_config => FormatterConfig},
            {ok, State};
        {error, Reason} ->
            {error, {gen_udp_open_failed, Reason}}
    end.

