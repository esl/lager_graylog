-module(lager_graylog_gelf_formatter).

-export([format/2]).

-type severity_int() :: 0..7.
-type formattable_term() :: binary()
                          | string()
                          | atom()
                          | integer()
                          | float()
                          | reference()
                          | port()
                          | pid().
-type key() :: formattable_term().
-type val() :: formattable_term().
-type kv() :: {key(), val()}.

-define(GELF_VERSION, "1.1").

%% API

-spec format(lager_msg:lager_msg(), list()) -> iodata().
format(Message, Opts) ->
    {ok, Host} = inet:gethostname(),
    ShortMessage = lager_msg:message(Message),
    Timestamp = erlang_ts_to_gelf_ts(lager_msg:timestamp(Message)),
    Level = severity_to_int(lager_msg:severity(Message)),
    Metadata = extract_metadata(lager_msg:metadata(Message), Opts),
    format(Host, ShortMessage, Timestamp, Level, Metadata).

%% Helpers

-spec extract_metadata([kv()], list()) -> [kv()].
extract_metadata(AllMetadata, Opts) ->
    case proplists:get_value(metadata, Opts, all) of
        all ->
            AllMetadata;
        Keys when is_list(Keys) ->
            lists:foldl(fun(K, Acc) ->
                            case proplists:lookup(K, AllMetadata) of
                                {K, _} = Tuple -> [Tuple | Acc];
                                _ -> Acc
                            end
                        end, [], Keys)
    end.

-spec format(string(), list(), float(), severity_int(), [kv()]) -> iodata().
format(Host, ShortMessage, Timestamp, Level, Metadata) ->
    Props = [{<<"version">>, ?GELF_VERSION},
             {<<"host">>, Host},
             {<<"short_message">>, ShortMessage},
             {<<"timestamp">>, Timestamp},
             {<<"level">>, Level} | format_metadata_keys(Metadata)],
    [${, lists:join($,, [format_prop(K, V) || {K, V} <- Props]) ,$}].

-spec format_metadata_keys([kv()]) -> [{iodata(), val()}].
format_metadata_keys(Metadata) ->
    [{format_metadata_key(K), V} || {K, V} <- Metadata].

-spec format_metadata_key(key()) -> iodata().
format_metadata_key(Key) ->
    [$_, term_to_bin(Key)].

-spec term_to_bin(formattable_term()) -> binary().
term_to_bin(Term) when is_binary(Term) -> Term;
term_to_bin(Term) when is_list(Term) -> list_to_binary(Term);
term_to_bin(Term) when is_atom(Term) -> atom_to_binary(Term, utf8);
term_to_bin(Term) when is_integer(Term) -> integer_to_binary(Term);
term_to_bin(Term) when is_float(Term) -> float_to_binary(Term, [{decimals, 6}, compact]);
term_to_bin(Term) when is_reference(Term) -> list_to_binary(erlang:ref_to_list(Term));
term_to_bin(Term) when is_port(Term) -> list_to_binary(erlang:port_to_list(Term));
term_to_bin(Term) when is_pid(Term) -> list_to_binary(pid_to_list(Term)).


-spec format_prop(iodata(), val()) -> iodata().
format_prop(FormattedKey, Val) ->
    [$", FormattedKey, $", $:, format_val(Val)].

-spec format_val(val()) -> iodata().
format_val(Val) when is_integer(Val) orelse is_float(Val) ->
    term_to_bin(Val);
format_val(Val) ->
    [$", term_to_bin(Val), $"].

-spec severity_to_int(lager:log_level()) -> severity_int().
severity_to_int(none) -> 0;
severity_to_int(emergency) -> 0;
severity_to_int(alert) -> 1;
severity_to_int(critical) -> 2;
severity_to_int(error) -> 3;
severity_to_int(warning) -> 4;
severity_to_int(notice) -> 5;
severity_to_int(info) -> 6;
severity_to_int(debug) -> 7.

-spec erlang_ts_to_gelf_ts(erlang:timestamp()) -> float().
erlang_ts_to_gelf_ts({MegaSecs, Secs, MicroSecs}) ->
    (MegaSecs * 1000000) + Secs + (MicroSecs / 1000000).

