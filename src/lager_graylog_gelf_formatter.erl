-module(lager_graylog_gelf_formatter).

-export([
         init/1,
         format/3
        ]).

-export_type([option/0]).

-behaviour(lager_graylog_gelf_formatter_behaviour).

-type option() :: {metadata, all | [atom()], {module(), Function :: atom()}}
                | {include_timestamp, boolean()}
                | {override_host, string() | binary()}
                | {on_encode_failure, crash | string() | binary()}.

-define(GELF_VERSION, <<"1.1">>).

%% API

init(_Opts) -> undefined.

-spec format(lager_msg:lager_msg(), undefined, [option()]) -> iodata().
format(Message, _State, Opts) ->
	Host = get_host(Opts),
    ShortMessage = lager_msg:message(Message),
    Level = lager_graylog_gelf_utils:severity_to_int(lager_msg:severity(Message)),
    Metadata = extract_metadata(Message, Opts),
    Props0 = [{<<"version">>, ?GELF_VERSION},
              {<<"host">>, Host},
              {<<"short_message">>, iolist_to_binary(ShortMessage)},
              {<<"level">>, Level} |
              lager_graylog_gelf_utils:prepare_metadata(Metadata)],
    TsProps = timestamp_prop(lager_msg:timestamp(Message), Opts),
    Props1 = lists:flatten([TsProps | Props0]),
    case on_encode_failure(Opts) of
        crash ->
            encode(Props1);
        OnFailMessage ->
            safely_encode(Props1, OnFailMessage)
    end.

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
        true  ->
            [{<<"timestamp">>, lager_graylog_gelf_utils:erlang_ts_to_gelf_ts(Timestamp)}];
        false -> []
    end.

-spec extract_metadata(lager_msg:lager_msg(), [option()]) ->
                              [lager_graylog_gelf_utils:metadata_kv()].
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


-spec on_encode_failure([option()]) -> crash | binary().
on_encode_failure(Opts) ->
    case proplists:get_value(on_encode_failure, Opts, crash) of
        crash ->
            crash;
        OnFailMessage when is_binary(OnFailMessage) ->
            OnFailMessage;
        OnFailMessage when is_list(OnFailMessage) ->
            list_to_binary(OnFailMessage)
    end.

-spec encode([tuple()]) -> binary() | no_return().
encode(Props) ->
    jiffy:encode({Props}).

-spec safely_encode([tuple()], OnFailMessage :: binary()) -> binary().
safely_encode(Props, OnFailMessage) ->
    case catch encode(Props) of
        {error, _} ->
            Props1 = lists:keyreplace(<<"short_message">>, 1, Props, {<<"short_message">>,
                                      OnFailMessage}),
            encode(Props1);
        Encoded ->
            Encoded
    end.
