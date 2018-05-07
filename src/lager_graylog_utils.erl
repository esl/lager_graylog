-module(lager_graylog_utils).

-export([validate_loglevel/1]).
-export([parse_common_opts/1]).

-export_type([mask/0]).

-type mask() :: {mask, integer()}.

%% API

-spec validate_loglevel(any()) -> {ok, mask()} | error.
validate_loglevel(Level) ->
    try lager_util:config_to_mask(Level) of
        Mask ->
            {ok, Mask}
    catch
        _:_ ->
            error
    end.

-spec parse_common_opts([{atom(), any()}]) -> {ok, map()} | {error, term()}.
parse_common_opts(Opts) when is_list(Opts) ->
    Level        = proplists:get_value(level, Opts, info),
    Host         = proplists:get_value(host, Opts),
    Port         = proplists:get_value(port, Opts),
    Formatter    = proplists:get_value(formatter, Opts, lager_graylog_gelf_formatter),
    FormatterConfig = proplists:get_value(formatter_config, Opts, []),

    case validate_config_values([{level, Level}, {host, Host}, {port, Port},
                                 {formatter, Formatter}]) of
        ok ->
            Config = #{level => Level,
                       host => Host,
                       port => Port,
                       formatter => Formatter,
                       formatter_config => FormatterConfig},
            {ok, Config};
        Error ->
            Error
    end.

%% Helpers

-spec validate_config_values([{atom(), term()}]) -> ok | {error, term()}.
validate_config_values([]) ->
    ok;
validate_config_values([Config | Rest]) ->
    case validate_config_value(Config) of
        ok ->
            validate_config_values(Rest);
        Error ->
            Error
    end.

-spec validate_config_value({atom(), term()}) -> ok | {error, term()}.
validate_config_value({host, undefined}) ->
    {error, undefined_host};
validate_config_value({port, undefined}) ->
    {error, undefined_port};
validate_config_value({port, P}) when not is_integer(P) ->
    {error, {invalid_port, P}};
validate_config_value({port, P}) when P < 1 orelse P > 65536 ->
    {error, {invalid_port, P}};
validate_config_value({level, L}) ->
     case validate_loglevel(L) of
        {ok, _} ->
            ok;
        error ->
            {error, {invalid_loglevel, L}}
     end;
validate_config_value({formatter, Formatter}) when not is_atom(Formatter) ->
    {error, {invalid_formatter, Formatter}};
validate_config_value(_) ->
    ok.

