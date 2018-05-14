-module(lager_graylog_utils).

-export([validate_loglevel/1]).
-export([parse_common_opts/1]).

-export_type([common_config/0]).

-type common_config() :: #{level := lager_graylog:mask(),
                           host := lager_graylog:host(),
                           port := lager_graylog:port_number(),
                           address_family := lager_graylog:address_family(),
                           formatter := module(),
                           formatter_config := any()}.

%% API

-spec validate_loglevel(any()) -> {ok, lager_graylog:mask()} | error.
validate_loglevel(Level) ->
    try lager_util:config_to_mask(Level) of
        Mask ->
            {ok, Mask}
    catch
        _:_ ->
            error
    end.

-spec parse_common_opts([{atom(), any()}]) -> {ok, common_config()} | {error, term()}.
parse_common_opts(Opts) when is_list(Opts) ->
    Level           = proplists:get_value(level, Opts, info),
    Host            = proplists:get_value(host, Opts),
    Port            = proplists:get_value(port, Opts),
    AddressFamily   = proplists:get_value(address_family, Opts),
    Formatter    = proplists:get_value(formatter, Opts, lager_graylog_gelf_formatter),
    FormatterConfig = proplists:get_value(formatter_config, Opts, []),

    OptsWithDefaults = [{level, Level},
                        {host, Host},
                        {port, Port},
                        {address_family, AddressFamily},
                        {formatter, Formatter},
                        {formatter_config, FormatterConfig}],
    validate_config_values(OptsWithDefaults, #{}).

%% Helpers

-spec validate_config_values([{atom(), term()}], Acc :: map()) ->
    {ok, common_config()} | {error, term()}.
validate_config_values([], Acc) ->
    {ok, Acc};
validate_config_values([{K, V} | Rest], Acc) ->
    case validate_config_value(K, V) of
        ok ->
            validate_config_values(Rest, Acc#{K => V});
        {ok, NewVal} ->
            validate_config_values(Rest, Acc#{K => NewVal});
        Error ->
            Error
    end.

-spec validate_config_value(atom(), term()) -> ok | {ok, term()} | {error, term()}.
validate_config_value(host, undefined) ->
    {error, undefined_host};
validate_config_value(port, undefined) ->
    {error, undefined_port};
validate_config_value(port, P) when not is_integer(P) ->
    {error, {invalid_port, P}};
validate_config_value(port, P) when P < 1 orelse P > 65536 ->
    {error, {invalid_port, P}};
validate_config_value(address_family, Family) when Family =/= undefined,
                                                   Family =/= inet,
                                                   Family =/= inet6 ->
    {error, {invalid_address_family, Family}};
validate_config_value(level, L) ->
     case validate_loglevel(L) of
        {ok, Mask} ->
             {ok, Mask};
        error ->
            {error, {invalid_loglevel, L}}
     end;
validate_config_value(formatter, Formatter) when not is_atom(Formatter) ->
    {error, {invalid_formatter, Formatter}};
validate_config_value(_, _) ->
    ok.

