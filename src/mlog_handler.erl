%% Copyright (c) 2021 Nicolas Martyanoff <khaelin@gmail.com>.
%%
%% Permission to use, copy, modify, and/or distribute this software for any
%% purpose with or without fee is hereby granted, provided that the above
%% copyright notice and this permission notice appear in all copies.
%%
%% THE SOFTWARE IS PROVIDED "AS IS" AND THE AUTHOR DISCLAIMS ALL WARRANTIES
%% WITH REGARD TO THIS SOFTWARE INCLUDING ALL IMPLIED WARRANTIES OF
%% MERCHANTABILITY AND FITNESS. IN NO EVENT SHALL THE AUTHOR BE LIABLE FOR ANY
%% SPECIAL, DIRECT, INDIRECT, OR CONSEQUENTIAL DAMAGES OR ANY DAMAGES
%% WHATSOEVER RESULTING FROM LOSS OF USE, DATA OR PROFITS, WHETHER IN AN
%% ACTION OF CONTRACT, NEGLIGENCE OR OTHER TORTIOUS ACTION, ARISING OUT OF OR
%% IN CONNECTION WITH THE USE OR PERFORMANCE OF THIS SOFTWARE.

-module(mlog_handler).

-export([log/2]).

-export_type([config/0, device/0]).

-type config() :: #{device => device()}.

-type device() :: standard_error | syslog.

-spec log(logger:log_event(), logger:handler_config()) -> ok.
log(Event = #{meta := Metadata}, #{config := HandlerConfig} = Config) ->
  Device =
    case maps:get(device, HandlerConfig) of
      syslog ->
        syslog;
      standard_error ->
        Self = self(),
        case maps:get(gl, Metadata, group_leader()) of
          Self -> standard_error;
          GroupLeader -> GroupLeader
        end
    end,
  Message = format_event(Event, Config),
  io:format(Device, "~ts", [Message]).

-spec format_event(logger:log_event(), logger:handler_config()) ->
        unicode:chardata().
format_event(Event, #{formatter := {Formatter, FormatterConfig}}) ->
  format_event(Event, Formatter, FormatterConfig);
format_event(Event, _) ->
  FormatterConfig = #{legacy_header => false, single_line => true},
  format_event(Event, logger_formatter, FormatterConfig).

-spec format_event(logger:log_event(), module(), logger:formatter_config()) ->
        unicode:chardata().
format_event(Event, Formatter, FormatterConfig) ->
  try
    Formatter:format(Event, FormatterConfig)
  catch
    error:Reason:Stacktrace ->
      io:format("cannot format log event: ~tp~n~tp~n", [Reason, Stacktrace])
  end.
