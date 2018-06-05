# Changelog
All notable changes to this project will be documented in this file.

The format is based on [Keep a Changelog](http://keepachangelog.com/en/1.0.0/) and this project
adheres to [Semantic Versioning](http://semver.org/spec/v2.0.0.html).

## [1.1.0] - 2018-06-05
* lager backend sending log messages to Graylog over SSL/TLS

## [1.0.0] - 2018-05-14
### Added
* lager backend sending log messages to Graylog over TCP
* option to filter log metadata by keys
* option to filter, modify and add metadata log by specifying a callback module and function
* option to override `host` field sent in the GELF payload
* option to not include `timestamp` field in the GELF payload
* option to choose custom formatter
* validation of backends' configuration options
* a license

### Changed
* name of the UDP-based backend from `lager_udp_backend` to `lager_graylog_udp_backend`
* name of the option to select address family used when opening a socket from `inet_family` to
  `address_family`
* formatting of binaries and lists - now they are "pretty-printed" inside the GELF payload regardless
  of whether they represent valid text

### Fixed
* encoding of numbers in the GELF payload - now they are correctly represented as numbers and not as
  strings
* encoding of log severity
