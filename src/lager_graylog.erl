-module(lager_graylog).

-export_type([host/0, port_number/0, mask/0, chunk_size/0,
              backend_option/0, udp_backend_option/0]).

-type host() :: inet:hostname().
-type port_number() :: inet:port_number().
-type address_family() :: undefined | inet | inet6.
-type chunk_size() :: pos_integer().

%% log level mask - definition taken from lager
-type mask() :: {mask, integer()}.

-type backend_option() :: {host, host()}
                        | {port, port_number()}
                        | {address_family, address_family()}
                        | {level, lager:log_level()}
                        | {formatter, module()}
                        | {formatter_config, term()}.

-type udp_backend_option() :: backend_option() |
                              {chunk_size, chunk_size()}.
