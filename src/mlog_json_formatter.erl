%% Copyright (c) 2020-2021 Nicolas Martyanoff <khaelin@gmail.com>.
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

-module(mlog_json_formatter).

-export([format/4]).

-spec format(unicode:chardata(), logger:level(), logger:metadata(),
             mlog_formatter:config()) ->
        unicode:chardata().
format(String, Level, Metadata, _Config) ->
  Domain = maps:get(domain, Metadata, []),
  Time = maps:get(time, Metadata, erlang:system_time(microsecond)),
  IgnoredMetadata = [domain, time, % duplicate
                     error_logger, logger_formatter, report_cb, % useless
                     gl], % added by the logger
  Metadata2 = maps:without(IgnoredMetadata, Metadata),
  Msg = #{level => atom_to_binary(Level),
          domain => mlog_formatter:format_domain(Domain),
          time => Time,
          message => unicode:characters_to_binary(String),
          data => format_metadata(Metadata2)},
  Msg2 = case maps:find(event, Metadata) of
           {ok, Event} ->
             Msg#{event => mlog_formatter:format_event(Event)};
           error ->
             Msg
         end,
  [json:serialize(Msg2), $\n].

-spec format_metadata(logger:metadata()) -> logger:metadata().
format_metadata(Metadata) ->
  maps:fold(fun format_metadata_entry/3, #{}, Metadata).

-spec format_metadata_entry(atom(), term(), logger:metadata()) ->
        logger:metadata().
format_metadata_entry(file, Value, Acc) ->
  Acc#{file => unicode:characters_to_binary(Value)};
format_metadata_entry(mfa, {M, F, A}, Acc) ->
  Acc#{mfa => [atom_to_binary(M), atom_to_binary(F), A]};
format_metadata_entry(pid, Value, Acc) ->
  Acc#{pid => list_to_binary(pid_to_list(Value))};
format_metadata_entry(Key, Value, Acc) ->
  Data = mlog_formatter:format_metadata_value(Value),
  Acc#{Key => iolist_to_binary(Data)}.
