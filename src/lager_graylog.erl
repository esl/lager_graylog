-module(lager_graylog).

-export_type([host/0, port_number/0, mask/0, backend_option/0,
              address_family/0, transport/0, extra_connect_opts/0]).

-type host() :: inet:hostname().
-type port_number() :: inet:port_number().
-type address_family() :: undefined | inet | inet6.
-type transport() :: gen_tcp | ssl.
-type extra_connect_opts() :: [gen_udp:option()]
                            | [gen_tcp:option()]
                            | [ssl:tls_client_option()].

%% log level mask - definition taken from lager
-type mask() :: {mask, integer()}.

-type backend_option() :: {host, host()}
                        | {port, port_number()}
                        | {address_family, address_family()}
                        | {level, lager:log_level()}
                        | {formatter, module()}
                        | {formatter_config, term()}
                        | {transport, transport()}
                        | {extra_connect_opts, extra_connect_opts()}.
