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
	 formats_metadata_using_configured_function,
     overrides_host_if_configured_as_binary,
     overrides_host_if_configured_as_string,
     binary_metadata_formatting,
     atom_metadata_formatting,
     integer_metadata_formatting,
     float_metadata_formatting,
     reference_metadata_formatting,
     pid_metadata_formatting,
     list_metadata_formatting,
     tuple_metadata_formatting,
     map_metadata_formatting,
     bitstring_metadata_formatting,
     formats_iolist_message_correctly,
     on_encode_failure_crashes,
     on_encode_failure_returns_configured_string,
     on_encode_failure_returns_configured_binary,
     on_encode_failure_crashes_if_second_encode_crashes_too
    ].

%% Test cases

formats_log_with_mandatory_attributes(_Config) ->
    Message = "hello",
    Timestamp = erlang:timestamp(),
    {ok, Host} = inet:gethostname(),
    Log = lager_msg:new(Message, Timestamp, debug, [], []),

    Formatted = lager_graylog_gelf_formatter:format(Log, undefined, []),
    ct:pal("~s", [Formatted]),

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

    Formatted = lager_graylog_gelf_formatter:format(Log, undefined, []),

    Gelf = decode(Formatted),
    ?assertEqual(<<"mod">>, maps:get(<<"_module">>, Gelf)),
    ?assertEqual(99, maps:get(<<"_line">>, Gelf)).

formats_only_selected_metadata(_Config) ->
    Log = lager_msg:new("hello", erlang:timestamp(), debug, [{module, mod}, {line, 99}], []),
    Opts = [{metadata, [module]}],

    Formatted = lager_graylog_gelf_formatter:format(Log, undefined, Opts),

    Gelf = decode(Formatted),
    ?assertEqual(<<"mod">>, maps:get(<<"_module">>, Gelf)),
    ?assertNot(maps:is_key(<<"_line">>, Gelf)).

formats_all_metadata_if_configured(_Config) ->
    Log = lager_msg:new("hello", erlang:timestamp(), debug, [{module, mod}, {line, 99}], []),
    Opts = [{metadata, all}],

    Formatted = lager_graylog_gelf_formatter:format(Log, undefined, Opts),

    Gelf = decode(Formatted),
    ?assertEqual(<<"mod">>, maps:get(<<"_module">>, Gelf)),
    ?assertEqual(99, maps:get(<<"_line">>, Gelf)).

doesnt_format_default_timestamp_if_configured(_Config) ->
    Log = lager_msg:new("hello", erlang:timestamp(), debug, [], []),
    Opts = [{include_timestamp, false}],

    Formatted = lager_graylog_gelf_formatter:format(Log, undefined, Opts),

    Gelf = decode(Formatted),
    ?assertNot(maps:is_key(<<"timestamp">>, Gelf)).

formats_metadata_using_configured_function(_Config) ->
    Log = lager_msg:new("hello", erlang:timestamp(), debug, [], []),
    Opts = [{metadata, {?MODULE, metadata_fun}}],

    Formatted = lager_graylog_gelf_formatter:format(Log, undefined, Opts),

    Gelf = decode(Formatted),
    ?assertEqual(<<"\"sample\"">>, maps:get(<<"_meta">>, Gelf)).

overrides_host_if_configured_as_binary(_Config) ->
    Log = lager_msg:new("hello", erlang:timestamp(), debug, [], []),
    Host = <<"some-host">>,
    Opts = [{override_host, Host}],

    Formatted = lager_graylog_gelf_formatter:format(Log, undefined, Opts),

    Gelf = decode(Formatted),
    ?assertEqual(Host, maps:get(<<"host">>, Gelf)).

overrides_host_if_configured_as_string(_Config) ->
    Log = lager_msg:new("hello", erlang:timestamp(), debug, [], []),
    Host = "some-host",
    Opts = [{override_host, Host}],

    Formatted = lager_graylog_gelf_formatter:format(Log, undefined, Opts),

    Gelf = decode(Formatted),
    ?assertEqual(list_to_binary(Host), maps:get(<<"host">>, Gelf)).


binary_metadata_formatting(_Config) ->
    assert_metadata_format([{<<"printable">>, <<"<<\"printable\">>">>},
                            {<<1, 2, 3>>, <<"<<1,2,3>>">>}]).

atom_metadata_formatting(_Config) ->
    assert_metadata_format([{'some-atom', <<"some-atom">>}]).

integer_metadata_formatting(_Config) ->
    assert_metadata_format([{-1, -1},
                            {1, 1},
                            {0, 0}]).

float_metadata_formatting(_Config) ->
    assert_metadata_format([{-1.12345, -1.12345},
                            {1.12345, 1.12345},
                            {0.0, 0.0}]).

reference_metadata_formatting(_Config) ->
    Ref = make_ref(),
    assert_metadata_format([{Ref, list_to_binary(erlang:ref_to_list(Ref))}]).

pid_metadata_formatting(_Config) ->
    Pid = spawn(fun() -> ok end),
    assert_metadata_format([{Pid, list_to_binary(pid_to_list(Pid))}]).

list_metadata_formatting(_Config) ->
    assert_metadata_format([{"string", <<"\"string\"">>},
                            {[1, 2, 3], <<"[1,2,3]">>},
                            {[], <<"[]">>},
                            {[48, [<<"hello">>], 21], <<"[48,[<<\"hello\">>],21]">>}]).

tuple_metadata_formatting(_Config) ->
    assert_metadata_format([{{}, <<"{}">>},
                            {{1}, <<"{1}">>},
                            {{"hello", there}, <<"{\"hello\",there}">>}]).

map_metadata_formatting(_Config) ->
    assert_metadata_format([{#{}, <<"#{}">>},
                            {#{key => val}, <<"#{key => val}">>}]).

bitstring_metadata_formatting(_Config) ->
    assert_metadata_format([{<<1:3>>, <<"<<1:3>>">>}]).

formats_iolist_message_correctly(_Config) ->
    IolistMsg = ["alice", ["has" | "a"], <<"cat">>, 20],
    Log = lager_msg:new(IolistMsg, erlang:timestamp(), debug, [], []),

    Formatted = lager_graylog_gelf_formatter:format(Log, undefined, []),

    Gelf = decode(Formatted),
    ?assertEqual(iolist_to_binary(IolistMsg), maps:get(<<"short_message">>, Gelf)).


on_encode_failure_crashes(_Config) ->
    Log = lager_msg:new(<<"strange character: ·">>, erlang:timestamp(), debug, [], []),
    Opts = [{on_encode_failure, crash}],

    ?assertThrow({error, {invalid_string, _}}, lager_graylog_gelf_formatter:format(Log, undefined, Opts)).

on_encode_failure_returns_configured_string(_Config) ->
    Log = lager_msg:new(<<"strange character: ·">>, erlang:timestamp(), debug, [], []),
    OnFailMessage = "Encoding GELF failed",
    Opts = [{on_encode_failure, OnFailMessage}],

    Formatted = lager_graylog_gelf_formatter:format(Log, undefined, Opts),

    Gelf = decode(Formatted),
    ?assertEqual(list_to_binary(OnFailMessage), maps:get(<<"short_message">>, Gelf)).

on_encode_failure_returns_configured_binary(_Config) ->
    Log = lager_msg:new(<<"strange character: ·">>, erlang:timestamp(), debug, [], []),
    OnFailMessage = <<"Encoding GELF failed">>,
    Opts = [{on_encode_failure, OnFailMessage}],

    Formatted = lager_graylog_gelf_formatter:format(Log, undefined, Opts),

    Gelf = decode(Formatted),
    ?assertEqual(OnFailMessage, maps:get(<<"short_message">>, Gelf)).

on_encode_failure_crashes_if_second_encode_crashes_too(_Config) ->
    Log = lager_msg:new(<<"strange character: ·">>, erlang:timestamp(), debug, [], []),
    Opts = [{on_encode_failure, <<"this message is invalid, too :( ·">>}],

    ?assertThrow({error, {invalid_string, _}}, lager_graylog_gelf_formatter:format(Log, undefined, Opts)).

metadata_fun(_) ->
    [{meta, "sample"}].

%% Helpers

-spec decode(iodata()) -> map().
decode(JSON) when is_binary(JSON) ->
    jiffy:decode(JSON, [return_maps]);
decode(JSON) ->
    decode(iolist_to_binary(JSON)).

-spec assert_same_timestamp(erlang:timestamp(), float()) -> ok | no_return().
assert_same_timestamp({MegaSecs, Secs, MicroSecs}, UnixTS) ->
    ?assertEqual((MegaSecs * 1000000) + Secs + (MicroSecs / 1000000), UnixTS),
    ok.

-spec assert_metadata_format([{term(), binary()}]) -> ok | no_return().
assert_metadata_format(ValuesWithExpectedFormat) ->
    MetadataWithExpectedFormat0 = lists:zip(lists:seq(1, length(ValuesWithExpectedFormat)),
                                            ValuesWithExpectedFormat),
    MetadataWithExpectedFormat = lists:map(fun({IntKey, {V, F}}) ->
                                               {list_to_atom(integer_to_list(IntKey)), {V, F}}
                                           end, MetadataWithExpectedFormat0),
    MetadataToLog = [{Key, Value} || {Key, {Value, _Format}} <- MetadataWithExpectedFormat],
    Log = lager_msg:new("hello", erlang:timestamp(), debug, MetadataToLog, []),
    Formatted = lager_graylog_gelf_formatter:format(Log, undefined, []),
    Gelf = decode(Formatted),
    lists:foreach(fun({Key, {_Value, ExpectedFormat}}) ->
                      GelfKey = iolist_to_binary(io_lib:format("_~s", [Key])),
                      ?assertEqual(ExpectedFormat, maps:get(GelfKey, Gelf))
                  end, MetadataWithExpectedFormat).
