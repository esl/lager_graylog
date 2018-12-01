# lager_graylog

[![Build Status](https://travis-ci.org/esl/lager_graylog.svg?branch=master)](https://travis-ci.org/esl/lager_graylog)

This application provides lager backends for sending log messages to [Graylog](https://www.graylog.org/)
over UDP or TCP, and the formatter module which spits out logs in [GELF](http://docs.graylog.org/en/stable/pages/gelf.html)
format.

## Installation

This library is [available on Hex](https://hex.pm/packages/lager_graylog). Just add it to your
dependencies!

```
{deps, [
  {lager_graylog, "1.1.0"}
]}.
```

## TCP backend

When you need a reliable log delivery, you can use the backend which connects to Graylog using TCP.
This backend will also try to reconnect to Graylog indefinitely (with backoff) in case of connection
failure.

To use it, just declare it in your lager config:

```erlang
[{lager, [{handlers, [{lager_graylog_tcp_backend, [{host, "graylog-hostname"},
                                                   {port, 12201}]

}]}]}].
```

This backend accepts configuration options as a list of tuples with atom keys. The following
options are supported:

* `host` (**required**) - host name or IP address (basically everything accepted by `gen_tcp:connect/3`)
   of the Graylog instance
* `port` (**required**) - port where the Graylog instance accepts TCP connection
* `level` (**optional**, default: `info`) - the log level (any log level supported by lager)
* `formatter` (**optional**, default: `lager_graylog_gelf_formatter`) - the formatter used for
  formatting log messages before sending to Graylog
* `formatter_config` - (**optional**, default: `[]`) - passed as a second argument to formatter's
  `format/2` function
* `address_family` (**optional**, default: `undefined`) - forces the backend to use specific IP
  protocol version. `inet` stands for IPv4, `inet6` for IPv6, and `undefined` means that suitable
  version will be chosen for you by the system. In most cases you won't need to set this option.

## UDP backend

If you care more about speed than reliability of delivery, you can use UDP-based backend.

To use it, just declare it in your lager config:

```erlang
[{lager, [{handlers, [{lager_graylog_tcp_backend, [{host, "graylog-hostname"},
                                                   {port, 12201}]

}]}]}].
```

It accepts exactly the same set of options as TCP backend.

### Chunking

UDP backend will try to avoid packet fragmentation with using of [chunking](http://docs.graylog.org/en/2.4/pages/gelf.html#chunking).
Chunk size in bytes can be configured with `chunk_size` option (**optional**, default: `1472` minimum: `300`),
According to GELF protocol, maximum number of allowed chunks is 128.
Please, keep in mind that each chunk also has a fixed 12 bytes headers part,
so maximum allowed size of log message equal to (`chunk_size` - 12) x 128.
If message will not fit in that size, it will be dropped and
error log with some debug info about initial message will be sent instead.

### Compression
This backend currently doesn't support [compression](http://docs.graylog.org/en/2.4/pages/gelf.html#compression).

## GELF formatter

The GELF formatter is implemented by the `lager_graylog_gelf_formatter` module. It formats log
messages according to GELF version 1.1. The following fields are always included in the message:

* `"version"` - always has value `"1.1"`
* `"host"` - hostname retrieved using `inet:gethostname/0` (might be overriden - see the
  `override_host` option in the configuration section below)
* `"short_message"` - the log message
* `"level"` - the log severity formatted as a number as in [syslog](https://en.wikipedia.org/wiki/Syslog#Severity_level)

### Metadata

All metadata provided by lager in the log message will be included as additional fields
(thus prefixed with `_` in the GELF payload). There are a couple things worth mentioning here:

* Metadata keys have to be either atoms, binaries or iolists (including simple strings).
* allowed characters in metadata keys are letters, numbers, underscores, dashes and dots. Note that
  the formatter won't validate the keys: make sure that they consist of valid characters or
  otherwise you might experience problems with Graylog.
* It is recommended to not include `id` metadata key  because Graylog server will ignore it anyway.

Below you can find the table showing how metadata values on the Erlang side map to the JSON values
in the GELF message sent to Graylog:

| type          | Erlang                                 | GELF                                      |
|---------------|----------------------------------------|-------------------------------------------|
| `atom()`      | `atom`                                 | `"atom"`                                  |
| `integer()`   | `23`                                   | `23`                                      |
| `float()`     | `1.23`                                 | `1.23`                                    |
| `binary()`    | `<<"alice has a cat">>`; `<<1, 2, 3>>` | `"<<\"alice has a cat\">>"; "<<1,2,3>>"`  |
| `list()`      | `"alice has a cat"`; `[1, 2, 3]`       | `"\"alice has a cat\""`; `"[1,2,3]"`      |
| `bitstring()` | `<<1:3>>`                              | `"<<1:3>>"`                               |
| `tuple()`     | `{1, 2, 3}`                            | `"{1,2,3}"`                               |
| `map()`       | `#{alice => "has a cat"}`              | `"#{alice => \"has a cat\"}"`             |
| `pid()`       | `<0.68.0>`                             | `"<0.68.0>"`                              |
| `reference()` | `#Ref<0.4168780290.2597847048.103549>` | `"#Ref<0.4168780290.2597847048.103549>"`  |


### Configuration

The formatter expects configuration as a list of tuples with atom keys. The following options are
supported (all of them are optional):
* `metadata` (default: `all`) - filters which metadata will be added to the message. You can provide
  `all` atom, which will instruct the formatter to include all metadata, or the list of keys
  (e.g. `[line, file]`) which must be included, or the `{module, function}` tuple. The function
  should take `lager_msg:lager_msg()` record as argument and return list of metadata tuples which
  will be included in the payload
* `include_timestamp` (default: `true`) - instructs the formatter to include the `"timestamp"` field
  containing the timestamp in seconds with 6 decimal places. Note that Graylog will generate timestamp
  itself when this field is not present in the payload
* `override_host` - if set, the `"host"` field in the GELF message will have the configured value
* `on_encode_failure` (default: `crash`) - controls the behaviour of the formatter when encoding fails.
  Possible values are:
  * `crash` - formatting throws an error
  * string or binary - formatter replaces given log message with configured value and retries the
    formatting. Note that if the second attempt fails, the error will be thrown anyway

Example configuration:

```erlang
[{metadata, all},
 {include_default_ts, false},
 {override_host, "my-chosen-hostname"},
 {on_encode_failure, "Encoding log message as GELF failed"}]
```

## Contributing

Pull requests are most welcome. If you have any questions, bug reports or feature proposals just
[open an issue](https://github.com/esl/lager_graylog/issues/new).

The project is developed using rebar3:
* run `rebar3 compile` to compile the source code
* run `rebar3 ct` to test it
* run `rebar3 dialyzer` to perform Dialyzer checks


Any new piece of code should be reasonably tested and covered by type specs. And again, if you have
any questions or are struggling with implementing a new feature, just open an issue and we'll try
to help :slightly_smiling_face:.

## License

Copyright 2018 Erlang Solutions

Licensed under the Apache License, Version 2.0 (the "License");
you may not use this file except in compliance with the License.
You may obtain a copy of the License at

    http://www.apache.org/licenses/LICENSE-2.0

Unless required by applicable law or agreed to in writing, software
distributed under the License is distributed on an "AS IS" BASIS,
WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
See the License for the specific language governing permissions and
limitations under the License.
