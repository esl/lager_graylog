-module(lager_graylog).

-export_type([host/0, port_number/0, mask/0]).

-type host() :: inet:hostname().
-type port_number() :: inet:port_number().

%% log level mask - definition taken from lager
-type mask() :: {mask, integer()}.

