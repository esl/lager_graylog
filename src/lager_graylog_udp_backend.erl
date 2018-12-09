-module(lager_graylog_udp_backend).

-behaviour(gen_event).

-include_lib("lager/include/lager.hrl").

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
                   chunk_size    := lager_graylog:chunk_size(),
                   machine_id    := binary(),
                   socket        := socket(),
                   formatter     := module(),
                   formatter_config := any(),
                   formatter_state := any(),
                   chunks_counter := non_neg_integer()
                  }.

-define(HEADERS_BYTE_SIZE, 12). %% 12 bytes reserved for headers.

%% gen_event callbacks

-spec init([lager_graylog:udp_backend_option()]) ->
    {ok, state()} | {error, {invalid_opts | gen_udp_open_failed, term()}}.
init(Opts) ->
    case lager_graylog_utils:parse_common_opts(Opts) of
        {ok, Config} ->
            case parse_opts(Opts) of
                {ok, UdpBackendConfig} ->
                    open_socket_and_init_state(maps:merge(Config, UdpBackendConfig));
                {error, Reason} ->
                    {error, {invalid_opts, Reason}}
            end;
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
                               formatter := Formatter,
                               formatter_config := FormatterConfig,
                               formatter_state := FormatterState
                              } = State) ->
    case lager_util:is_loggable(Message, Mask, Name) of
        true ->
            FormattedLog = Formatter:format(Message, FormatterState, FormatterConfig),
            case send(State, FormattedLog, byte_size(FormattedLog)) of
                {ok, NewState} -> {ok, NewState};
                {error, Error} ->
                    Metadata = lager_msg:metadata(Message),
                    DebugMeta =
                        [
                         {pid, proplists:get_value(pid, Metadata)},
                         {module, proplists:get_value(module, Metadata)},
                         {function, proplists:get_value(function, Metadata)},
                         {line, proplists:get_value(line, Metadata)}
                        ],
                    ?INT_LOG(error, "~p DebugMeta ~p", [Error, DebugMeta]),
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

send(#{host := Host, port := Port, socket := Socket, chunk_size := ChunkSize} = State,
     Msg, MsgSize) when MsgSize =< ChunkSize ->
    gen_udp:send(Socket, Host, Port, Msg),
    {ok, State};
send(#{chunk_size := ChunkSize, machine_id := MachineId, chunks_counter := ChunksCounter} = State,
     Msg, MsgSize) ->
    PayloadSize = ChunkSize - ?HEADERS_BYTE_SIZE,
    ChunksPerPayload = MsgSize / PayloadSize,
    WholeChunksNumber = trunc(ChunksPerPayload),
    ChunksNumber =
        case ChunksPerPayload - WholeChunksNumber == 0 of
            true  -> WholeChunksNumber;
            false -> WholeChunksNumber + 1
        end,
    case ChunksNumber > 128 of %% maximum number of chunks in GELF
        true -> {error, {message_to_big, [{msg_size, MsgSize}, {chunk_size, ChunkSize}]}};
        false ->
            {NewChunksCounter, ChunkId} = generate_chunk_id(MachineId, ChunksCounter),
            chunk_send(ChunkId, 0, ChunksNumber, PayloadSize, MsgSize, Msg, State),
            {ok, State#{chunks_counter => NewChunksCounter}}
    end.

chunk_send(_ChunkId, _SequenceNumber, _NumberOfChunks, _PayloadSize, _BodyLength, <<>>, _State) ->
    ok;
chunk_send(ChunkId, SequenceNumber, NumberOfChunks, PayloadSize, BodyLength, Body,
           #{host := Host, port := Port, socket := Socket} = State) ->
    CurrentPayloadSize = erlang:min(PayloadSize, BodyLength),
    <<BodyPart:CurrentPayloadSize/binary, Rest/binary>> = Body,
    ChunkData = <<30,15, %% Chunked GELF magic bytes
                  ChunkId/binary,
                  SequenceNumber:8/integer,
                  NumberOfChunks:8/integer,
                  BodyPart/binary>>,
    gen_udp:send(Socket, Host, Port, ChunkData),
    chunk_send(ChunkId, SequenceNumber + 1, NumberOfChunks, PayloadSize,
               BodyLength - CurrentPayloadSize, Rest, State).

-spec open_socket_and_init_state(map()) -> {ok, state()} | {error, {gen_udp_open_failed, term()}}.
open_socket_and_init_state(Config) ->
    #{level := Mask,
      host := Host,
      port := Port,
      address_family := AddressFamily,
      chunk_size := ChunkSize,
      formatter := Formatter,
      formatter_config := FormatterConfig} = Config,
    case gen_udp:open(0, [binary, {active, false} | extra_open_opts(AddressFamily)]) of
        {ok, Socket} ->
            State = #{name => {?MODULE, {Host, Port}},
                      level => Mask,
                      host => Host,
                      port => Port,
                      socket => Socket,
                      chunk_size => ChunkSize,
                      machine_id => crypto:strong_rand_bytes(4),
                      chunks_counter => 0,
                      formatter => Formatter,
                      formatter_config => FormatterConfig,
                      formatter_state => Formatter:init(FormatterConfig)
                     },
            {ok, State};
        {error, Reason} ->
            {error, {gen_udp_open_failed, Reason}}
    end.

generate_chunk_id(MachineId, ChunksCounter) ->
    ChunkId = <<MachineId/binary, ChunksCounter:4/unsigned-integer-unit:8>>,
    NewChunksCounter =
        case ChunksCounter =:= 4294967295 of
            true  -> 0;
            false -> ChunksCounter + 1
        end,
    {NewChunksCounter, ChunkId}.

-spec extra_open_opts(lager_graylog:address_family()) -> [inet:address_family()].
extra_open_opts(undefined) -> [];
extra_open_opts(inet) -> [inet];
extra_open_opts(inet6) -> [inet6].

parse_opts(Opts) when is_list(Opts) ->
    %% 1472 looks like good default value to avoid fragmentation
    %% 1500 bytes - 20 bytes for the IPv4 header and 8 bytes for the UDP header
    ChunkSize = proplists:get_value(chunk_size, Opts, 1472),
    OptsWithDefaults = [{chunk_size, ChunkSize}],
    Errors =
        lists:foldl(
          fun({Opt, OptVal}, ErrorAcc) ->
                  case validate_opt(Opt, OptVal) of
                      ok    -> ErrorAcc;
                      Error -> [Error | ErrorAcc]
                  end
          end, [], OptsWithDefaults),
    case Errors of
        [] -> {ok, maps:from_list(OptsWithDefaults)};
        _  -> {error, Errors}
    end.

validate_opt(chunk_size, ChunkSize) when ChunkSize < 300 -> {error, chunk_size_too_small};
validate_opt(_Opt, _OptVal) -> ok.
