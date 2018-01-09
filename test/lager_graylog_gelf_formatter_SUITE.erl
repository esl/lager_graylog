-module(lager_graylog_gelf_formatter_SUITE).

-include_lib("eunit/include/eunit.hrl").

-compile(export_all).

%% Suite configuration

all() ->
    [formats_log_with_mandatory_attributes,
     formats_all_metadata_by_default,
     formats_only_selected_metadata,
     formats_all_metadata_if_configured,
     doesnt_format_default_timestamp_if_configured,
	 formats_metadata_using_configured_function
    ].

%% Test cases

formats_log_with_mandatory_attributes(_Config) ->
    Message = "hello",
    Timestamp = erlang:timestamp(),
    {ok, Host} = inet:gethostname(),
    Log = lager_msg:new(Message, Timestamp, debug, [], []),

    Formatted = lager_graylog_gelf_formatter:format(Log, []),

    Gelf = decode(Formatted),
    ?assertEqual(<<"1.1">>, maps:get(<<"version">>, Gelf)),
    ?assertEqual(list_to_binary(Message), maps:get(<<"short_message">>, Gelf)),
    ?assertEqual(list_to_binary(Host), maps:get(<<"host">>, Gelf)),
    ?assertEqual(7, maps:get(<<"level">>, Gelf)),
    UnixTS = maps:get(<<"timestamp">>, Gelf),
    ?assert(is_float(UnixTS)),
    assert_same_timestamp(Timestamp, UnixTS).

formats_all_metadata_by_default(_Config) ->
    Log = lager_msg:new("hello", erlang:timestamp(), debug, [{module, mod}, {line, 99}], []),

    Formatted = lager_graylog_gelf_formatter:format(Log, []),

    Gelf = decode(Formatted),
    ?assertEqual(<<"mod">>, maps:get(<<"_module">>, Gelf)),
    ?assertEqual(99, maps:get(<<"_line">>, Gelf)).

formats_only_selected_metadata(_Config) ->
    Log = lager_msg:new("hello", erlang:timestamp(), debug, [{module, mod}, {line, 99}], []),
    Opts = [{metadata, [module]}],

    Formatted = lager_graylog_gelf_formatter:format(Log, Opts),

    Gelf = decode(Formatted),
    ?assertEqual(<<"mod">>, maps:get(<<"_module">>, Gelf)),
    ?assertNot(maps:is_key(<<"_line">>, Gelf)).

formats_all_metadata_if_configured(_Config) ->
    Log = lager_msg:new("hello", erlang:timestamp(), debug, [{module, mod}, {line, 99}], []),
    Opts = [{metadata, all}],

    Formatted = lager_graylog_gelf_formatter:format(Log, Opts),

    Gelf = decode(Formatted),
    ?assertEqual(<<"mod">>, maps:get(<<"_module">>, Gelf)),
    ?assertEqual(99, maps:get(<<"_line">>, Gelf)).

doesnt_format_default_timestamp_if_configured(_Config) ->
    Log = lager_msg:new("hello", erlang:timestamp(), debug, [], []),
    Opts = [{include_timestamp, false}],

    Formatted = lager_graylog_gelf_formatter:format(Log, Opts),

    Gelf = decode(Formatted),
    ?assertNot(maps:is_key(<<"timestamp">>, Gelf)).

formats_metadata_using_configured_function(_Config) ->
    Log = lager_msg:new("hello", erlang:timestamp(), debug, [], []),
    Opts = [{metadata, {?MODULE, metadata_fun}}],

    Formatted = lager_graylog_gelf_formatter:format(Log, Opts),

    Gelf = decode(Formatted),
    ?assertEqual(<<"sample">>, maps:get(<<"_meta">>, Gelf)).

metadata_fun(_) ->
    [{meta, "sample"}].

%% Helpers

-spec decode(iodata()) -> map().
decode(JSON) when is_binary(JSON) ->
    jsx:decode(JSON, [return_maps]);
decode(JSON) ->
    decode(iolist_to_binary(JSON)).

-spec assert_same_timestamp(erlang:timestamp(), float()) -> ok | no_return().
assert_same_timestamp({MegaSecs, Secs, MicroSecs}, UnixTS) ->
    ?assertEqual((MegaSecs * 1000000) + Secs + (MicroSecs / 1000000), UnixTS),
    ok.

