% erl-mlog

# Introduction
The erl-mlog project aims to provide a minimal usable configuration for the
standard [`logger`](https://erlang.org/doc/man/logger.html) application.

# Features
## Handling
The `mlog_handler` module is as simple as possible: log events are formatted
using the log formatter specified in the configuration and written to the
group leader associated with the event (the `gl` value in metadata).

The use of the `gl` metadata value is the main difference with the standard
`logger_std_h`. See [this email
thread](https://erlang.org/pipermail/erlang-questions/2021-May/100992.html)
for more information about the rational behind this choice.

## Formatting
The `mlog_formatter` module provides a human-readable text output and a JSON
output.

## Features
## Metadata
The following metadata entry are handled separately:
- `domain`: see the [official
  documentation](https://erlang.org/doc/man/logger_filters.html#domain-2) for
  more information.
- `event`: a list of atoms identifying what is being logged.

## Text output
In text mode, the formatter returns one or more lines of text representing the
log message. Messages are prefixed with the log level, domain string (if
present) and event string (if present). If metadata are associated with the
message, they are printed on a separate line, formatted as a sequence of
"key=value" strings.

## JSON output
In JSON mode, the formatter returns a JSON object serialized as a single line
of text for each log message.

The following members are used:
- `level`: the log level string (e.g. `"error"`);
- `domain`: the
  [domain](https://erlang.org/doc/man/logger_filters.html#domain-2) of the log
  message formatted as a string (e.g. `"otp.sasl"`). Optional.
- `event`: if the message has an `event` metadata entry, a string representing
  the event. For example, if event was `[http, request_received]`, the field
  will be formatted as `http.request_received`. Optional.
- `time`: the timestamp of the message as a microsecond UNIX timestamp.
- `message`: the message string.
- `data`: an object containing metadata associated with the message.

## Configuration
The following options are available:
- `debug`: print log events on the default output; used when developing the
  formatter itself.
- `format`: the output format, either `text` or `json`.
- `include_time`: include the time in each formatted message (`text` format
  only).
- `color`: colorize log entries (`text` format only).

# Default configuration
The `mlog:install/0` function can be used to install a default configuration
for the `logger` application. Starting the `mlog` application will call
`mlog:install/0`.

With this default configuration, two handlers are created:

- `default` logs all messages with a level higher or equal to `info` are
  logged (instead of higher or equal to `notice` for the default `logger`
  configuration). It also drops:
  - Messages from remote nodes.
  - Progress messages from `supervisor` and `application_controller` since
    their utility is debatable.
  - SASL messages: they tend to be extremely verbose and their utility is
    debatable.
- `debug` only accept messages with a `debug` level and blocks all of them by
  default. The user can provide a list of additional filters with the
  `debug_filters` option. For example, to log all messages in a sub-domain of
  `otp`, add the `{otp, {fun logger_filters:domain/2, {log, sub, [otp]}}}`
  filter.

## Options
The default configuration used options from the application environment.
The following options are available:

- `formatter`: the `mlog_formatter` configuration used by all handlers.
- `debug_filters`: a list of [logger
  filters](https://erlang.org/doc/apps/kernel/logger_chapter.html#filters)
  applied to the debug handler.
