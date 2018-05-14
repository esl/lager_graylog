-module(lager_graylog_gelf_formatter).

-export([format/2]).

-export_type([option/0]).

-type option() :: {metadata, all | [atom()], {module(), Function :: atom()}}
                | {include_timestamp, boolean()}
                | {override_host, string() | binary()}.

-type severity_int() :: 0..7.
-type metadata_key() :: atom().
-type metadata_val() :: binary()
             | atom()
             | integer()
             | float()
             | reference()
             | port()
             | pid()
             | list()
             | tuple()
             | map()
             | bitstring().
-type metadata_kv() :: {metadata_key(), metadata_val()}.

-define(GELF_VERSION, <<"1.1">>).

%% API

-spec format(lager_msg:lager_msg(), [option()]) -> iodata().
format(Message, Opts) ->
	Host = get_host(Opts),
    ShortMessage = lager_msg:message(Message),
    Level = severity_to_int(lager_msg:severity(Message)),
    Metadata = extract_metadata(Message, Opts),
    Props0 = [{<<"version">>, ?GELF_VERSION},
              {<<"host">>, Host},
              {<<"short_message">>, iolist_to_binary(ShortMessage)},
              {<<"level">>, Level} | prepare_metadata(Metadata)],
    TsProps = timestamp_prop(lager_msg:timestamp(Message), Opts),
    Props1 = lists:flatten([TsProps | Props0]),
    jiffy:encode({Props1}).

%% Helpers

-spec get_host([option()]) -> binary().
get_host(Opts) ->
    Host =
        case proplists:lookup(override_host, Opts) of
            {override_host, OverridenHost} ->
                OverridenHost;
            _ ->
                {ok, InetHost} = inet:gethostname(),
                InetHost
        end,
    iolist_to_binary(io_lib:format("~s", [Host])).

-spec timestamp_prop(erlang:timestamp(), [option()]) -> [{binary(), float()}].
timestamp_prop(Timestamp, Opts) ->
    case proplists:get_value(include_timestamp, Opts, true) of
       true  -> [{<<"timestamp">>, erlang_ts_to_gelf_ts(Timestamp)}];
       false -> []
    end.

-spec extract_metadata(lager_msg:lager_msg(), [option()]) -> [metadata_kv()].
extract_metadata(Message, Opts) ->
	AllMetadata = lager_msg:metadata(Message),
    case proplists:get_value(metadata, Opts, all) of
        all ->
            AllMetadata;
        {Mod, Fun} when is_atom(Mod) andalso is_atom(Fun) ->
	        Mod:Fun(Message);
        Keys when is_list(Keys) ->
            lists:foldl(fun(K, Acc) ->
                            case proplists:lookup(K, AllMetadata) of
                                {K, _} = Tuple -> [Tuple | Acc];
                                _ -> Acc
                            end
                        end, [], Keys)
    end.

-spec prepare_metadata([metadata_kv()]) -> [{binary(), binary() | atom() | number()}].
prepare_metadata(Metadata) ->
    [{prepare_metadata_key(K), prepare_metadata_val(V)} || {K, V} <- Metadata].

-spec prepare_metadata_key(metadata_key()) -> binary().
prepare_metadata_key(Key) ->
    iolist_to_binary(io_lib:format("_~s", [Key])).

-spec prepare_metadata_val(metadata_val()) -> binary() | atom() | number().
prepare_metadata_val(Term) when is_atom(Term) -> Term;
prepare_metadata_val(Term) when is_number(Term) -> Term;
prepare_metadata_val(Term) ->
    iolist_to_binary(io_lib:format("~p", [Term])).

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

