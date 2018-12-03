-module(lager_graylog_formatter).

%% Called once on backend process start.
%% Returned value will be stored in process state and passed to
%% each format call.
-callback init(Options :: any()) -> FormatterState :: term().

-callback format(lager_msg:lager_msg(), FormatterState :: term(), Options :: any()) ->
    FormattedMessage :: iodata().
