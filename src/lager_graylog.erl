-module(lager_graylog).

-export_type([host/0, port_number/0, mask/0, backend_option/0]).

-type host() :: inet:hostname().
-type port_number() :: inet:port_number().
-type address_family() :: undefined | inet | inet6.

%% log level mask - definition taken from lager
-type mask() :: {mask, integer()}.

-type backend_option() :: {host, host()}
                        | {port, port_number()}
                        | {address_family, address_family()}
                        | {level, lager:log_level()}
                        | {formatter, module()}
                        | {formatter_config, term()}.
