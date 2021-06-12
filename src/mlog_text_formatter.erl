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

-module(mlog_text_formatter).

-export([format/4]).

-type column() :: pos_integer().

-type tag() :: term().
-type tag_table() :: #{tag() := pos_integer()}.

-type part() :: #{text := unicode:chardata(),
                  color => term:color(),
                  width => pos_integer()}
              | {spacer, pos_integer()}
              | {tag, tag()}
              | {align, tag()}
              | eol.
-type line() :: [part()].

-spec format(unicode:chardata(), logger:level(), logger:metadata(),
             mlog_formatter:config()) ->
        unicode:chardata().
format(Message, Level, Metadata, Config) ->
  LevelPart = #{text => mlog_formatter:format_level(Level),
                width => 9},
  DomainPart = case maps:find(domain, Metadata) of
                 {ok, Domain} ->
                   #{text => mlog_formatter:format_domain(Domain),
                     color => green,
                     width => 24};
                 error ->
                   undefined
               end,
  TimePart = case maps:get(include_time, Config, false) of
               true ->
                 Time = case maps:find(time, Metadata) of
                          {ok, T} -> T;
                          error -> erlang:system_time(microsecond)
                        end,
                 #{text => format_time(Time)};
               false ->
                 undefined
             end,
  EventPart = case maps:find(event, Metadata) of
                {ok, Event} ->
                  #{text => [$[, mlog_formatter:format_event(Event), $]],
                    color => yellow};
                error ->
                  undefined
              end,
  MessagePart = #{text => string:trim(Message, trailing, " \n\t")},
  FullMessagePart = case EventPart of
                      undefined ->
                        [{tag, message}, MessagePart];
                      _ ->
                        [{tag, message}, EventPart, {spacer, 1}, MessagePart]
                    end,
  BaseParts0 = [TimePart, LevelPart, DomainPart, FullMessagePart],
  BaseParts = lists:join({spacer, 1}, [P || P <- BaseParts0, P =/= undefined]),
  MetadataParts = metadata_parts(Metadata),
  Line = lists:flatten(BaseParts) ++ [{spacer, 1}] ++ MetadataParts,
  [format_line(Line, Config), $\n].

-spec format_line(line(), mlog_formatter:config()) -> unicode:chardata().
format_line(Line, Config) ->
  format_parts(Line, [], 1, #{}, Config).

-spec format_parts([part()], [unicode:chardata()], column(), tag_table(),
                   mlog_formatter:config()) ->
        unicode:chardata().
format_parts([], Acc, _, _, _) ->
  lists:reverse(Acc);
format_parts([Part | Parts], Acc, Column, Tags, Config) ->
  {String, Column2, Tags2} = format_part(Part, Column, Tags, Config),
  format_parts(Parts, [[String] | Acc], Column2, Tags2, Config).

-spec format_part(part(), column(), tag_table(), mlog_formatter:config()) ->
        {unicode:chardata(), column(), tag_table()}.
format_part(eol, _Column, Tags, _Config) ->
  {"\n", 1, Tags};
format_part({spacer, Width}, Column, Tags, _Config) ->
  {lists:duplicate(Width, $\s), Column+Width, Tags};
format_part({tag, Tag}, Column, Tags, _Config) ->
  {"", Column, Tags#{Tag => Column}};
format_part({align, Tag}, Column, Tags, _Config) ->
  case maps:find(Tag, Tags) of
    {ok, TagColumn} when Column < TagColumn ->
      Width = TagColumn - Column,
      {lists:duplicate(Width, $\s), Column+Width, Tags};
    _ ->
      {"", Column, Tags}
  end;
format_part(Part = #{text := Text}, Column, Tags, Config) ->
  PaddedText = case maps:find(width, Part) of
                 {ok, Width} ->
                   pad_text(Text, Width);
                 error ->
                   Text
               end,
  {IndentedText, Column2} = indent_text(PaddedText, Column),
  ColorizedText = case maps:find(color, Part) of
                    {ok, Color} ->
                      maybe_colorize(IndentedText, Color, Config);
                    error ->
                      IndentedText
                  end,
  {ColorizedText, Column2, Tags}.

-spec pad_text(unicode:chardata(), pos_integer()) -> unicode:chardata().
pad_text(Text, Width) ->
  Lines = [[string:pad(Line, Width)] || Line <- string:split(Text, "\n")],
  lists:join($\n, Lines).

-spec indent_text(unicode:chardata(), column()) ->
        {unicode:chardata(), column()}.
indent_text(Text, Column) ->
  Padding = lists:duplicate(Column-1, $\s),
  [FirstLine | OtherLines] = string:split(Text, "\n", all),
  IndentedLines = [[Padding, Line] || Line <- OtherLines],
  Lines2 = [FirstLine | IndentedLines],
  LastLine = lists:last(Lines2),
  Text2 = lists:join($\n, Lines2),
  {Text2, Column + string:length(LastLine)}.

-spec maybe_colorize(unicode:chardata(), term:color(),
                     mlog_formatter:config()) ->
        unicode:chardata().
maybe_colorize(String, Color, Config) ->
  case maps:get(color, Config, true) of
    true ->
      term:with_text_attributes(String, [{foreground, Color}]);
    false ->
      String
  end.

-spec format_time(SystemTime :: integer()) -> unicode:chardata().
format_time(SystemTime) ->
  calendar:system_time_to_rfc3339(SystemTime, [{unit, microsecond},
                                               {offset, "Z"}]).

-spec metadata_parts(logger:metadata()) -> [part()].
metadata_parts(Metadata0) ->
  Ignored = [domain, time, event, % duplicate
             error_logger, logger_formatter, report_cb, % useless
             mfa, file, line, % added by log macros
             pid, gl], % added by the logger
  Metadata = maps:without(Ignored, Metadata0),
  if
    map_size(Metadata) =:= 0 ->
      [];
    true ->
      Parts = [[#{text => atom_to_binary(Name), color => blue},
                #{text => "="},
                #{text => format_metadata_value(Value)}]
               || {Name, Value} <- maps:to_list(Metadata)],
      [eol, {align, message}] ++ lists:flatten(lists:join({spacer, 1}, Parts))
  end.

-spec format_metadata_value(term()) -> unicode:chardata().
format_metadata_value(Value) ->
  Data = mlog_formatter:format_metadata_value(Value),
  quote_string(iolist_to_binary(Data)).

-spec quote_string(binary()) -> unicode:chardata().
quote_string(String) ->
  [$", re:replace(String, "[\\\\\"]", "\\\\&", [global]), $"].
