-module(lager_graylog_tcp_backend).

-behaviour(gen_event).

-export([init/1]).
-export([handle_call/2]).
-export([handle_event/2]).
-export([handle_info/2]).
-export([terminate/2]).
-export([code_change/3]).

-type host() :: inet:hostname().
-type port_number() :: inet:port_number().
-type name() :: {?MODULE, {host(), port_number()}}.
-type state() :: #{name          := name(),
                   level         := lager_graylog_utils:mask(),
                   host          := host(),
                   port          := port_number(),
                   socket        := gen_tcp:socket(),
                   formatter     := module(),
                   format_config := any()}.

%% gen_event callbacks

init(Opts) ->
    #{level := Level,
      host := Host,
      port := Port,
      formatter := Formatter,
      format_config := FormatConfig} = get_common_config(Opts),

    {ok, Socket} = gen_tcp:connect(Host, Port, [binary, {active, false}]),

    {ok, Mask} = lager_graylog_utils:validate_loglevel(Level),
    State = #{name => {?MODULE, {Host, Port}},
              level => Mask,
              host => Host,
              port => Port,
              socket => Socket,
              formatter => Formatter,
              format_config => FormatConfig},
    {ok, State}.

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
                               socket := Socket,
                               formatter := Formatter,
                               format_config := FormatConfig} = State) ->
    case lager_util:is_loggable(Message, Mask, Name) of
        true ->
            FormattedLog = Formatter:format(Message, FormatConfig),
            ok = gen_tcp:send(Socket, [FormattedLog, 0]);
        false ->
            ok
    end,
    {ok, State}.

handle_info(Info, State) ->
    {ok, State}.

terminate(_Arg, _State) ->
    ok.

code_change(_OldVsn, State, _Extra) ->
    {ok, State}.

%% Helpers

-spec get_common_config(any()) -> map() | no_return().
get_common_config(Opts) ->
    case lager_graylog_utils:parse_common_opts(Opts) of
        {ok, Config} ->
            Config;
        Error ->
            exit(Error)
    end.

