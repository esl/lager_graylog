## lager_graylog

This application provides lager backends for sending log messages to [Graylog](https://www.graylog.org/)
over UDP or TCP, and the formatter module which spits out logs in [GELF](http://docs.graylog.org/en/stable/pages/gelf.html)
format.

### TCP backend

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

### GELF formatter

The GELF formatter is implemented by the `lager_graylog_gelf_formatter` module. It formats log
messages according to GELF version 1.1. The following fields are always included in the message:

* `"version"` - always has value `"1.1"`
* `"host"` - hostname retrieved using `inet:gethostname/0`
* `"short_message"` - the log message
* `"level"` - the log severity formatted as a number as in [syslog](https://en.wikipedia.org/wiki/Syslog#Severity_level)

In addition, all metadata provided by lager in the log message will be included as additional fields
(thus prefixed with `_`). Please note that this formatter can only format binaries, strings, atoms,
integers, floats (always formatted with 6 decimal places), references, ports and pids.

#### Configuration

The formatter expects configuration as a list of tuples with atom keys. The following options are
supported (all of them are optional):
* `metadata` (default: `all`) - filters which metadata will be added to the message. You can provide
  `all` atom, which will instruct the formatter to include all metadata, or the list of keys
  (e.g. `[line, file]`) which must be included
* `include_default_ts` (default: `true`) - instructs the formatter to include the `"timestamp"` field
  containing the timestamp in seconds with 6 decimal places. Note that Graylog will generate timestamp
  itself when this field is not present in the payload

Example configuration:

```erlang
[{metadata, all}, {include_default_ts, false}]
```

