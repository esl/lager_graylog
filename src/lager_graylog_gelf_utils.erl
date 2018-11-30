-module(lager_graylog_gelf_utils).

-export([
         prepare_metadata/1,
         prepare_metadata_key/1,
         prepare_metadata_val/1,
         severity_to_int/1,
         erlang_ts_to_gelf_ts/1
        ]).

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
