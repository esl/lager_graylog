Lager graylog udp
=================

There are two modules:
* lager\_gelf\_formatter, which will format your logs according to gelf
standards
* graylog\_udp\_sender, which will send your logs to graylog instance

Config
======

```
[{lager, [
    {handlers, [
        {lager_udp_backend,
            [info,
            {host, "graylog.erlang-solutions.com"},
            {port, 12201},
            {level, info},
            {formatter, lager_gelf_formatter},
            {formatter_config, [{metadata, [{service, "SERVICE NAME"}]}]}
            ]}
        ]}
    ]}].
```
