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

-module(mlog_formatter).

-export([format/2, format_level/1, format_domain/1, format_event/1,
         format_metadata_value/1]).

-export_type([format/0, config/0, msg/0]).

-type format() :: text | json.

-type config() ::
        #{debug => boolean(),
          format => format(),
          include_time => boolean(),
          color => boolean(),
          print_width => pos_integer(),
          max_depth => pos_integer(),
          max_line_Length => pos_integer()}.

-type msg() ::
        {io:format(), [term()]}
      | {report, logger:report()}
      | {string, unicode:chardata()}.

-spec format(logger:log_event(), config()) ->
        unicode:chardata().
format(Event = #{level := Level, msg := Msg, meta := Metadata}, Config) ->
  maybe_log(Event, Config),
  maybe_truncate(format_msg(Msg, Level, Metadata, Config), Config).

-spec maybe_log(logger:log_event(), config()) -> ok.
maybe_log(Event, #{debug := true}) ->
  io:format("log event:~n~p~n", [Event]);
maybe_log(_, _) ->
  ok.

-spec maybe_truncate(unicode:chardata(), config()) -> unicode:chardata().
maybe_truncate(String, #{max_line_Length := MaxLength0}) ->
  MaxLength = max(MaxLength0-3, 0),
  [string:slice(String, 0, MaxLength), "...\n"];
maybe_truncate(String, _) ->
  String.

-spec format_msg(msg(), logger:level(), logger:metadata(), config()) ->
        unicode:chardata().

format_msg({string, String}, Level, Metadata, Config) ->
  case maps:get(format, Config, text) of
    text ->
      mlog_text_formatter:format(String, Level, Metadata, Config);
    json ->
      mlog_json_formatter:format(String, Level, Metadata, Config);
    Format ->
      error({unknown_format, Format})
  end;

format_msg({report, Report}, Level, Metadata = #{report_cb := Fun}, Config) when
    is_function(Fun, 1) ->
  {FormatData, Args} = Fun(Report),
  Format = unicode:characters_to_binary(FormatData),
  format_msg({Format, Args}, Level, Metadata, Config);

format_msg({report, Report}, Level, Metadata = #{report_cb := Fun}, Config) when
    is_function(Fun, 2) ->
  String = Fun(Report, #{}),
  format_msg({string, String}, Level, Metadata, Config);

format_msg({report, Report}, Level, Metadata, Config) ->
  format_msg({report, Report}, Level,
             Metadata#{report_cb => fun logger:format_report/1},
             Config);

format_msg({Format, Args}, Level, Metadata, Config) ->
  String = format_string(Format, Args, Config),
  format_msg({string, String}, Level, Metadata, Config).

-spec format_level(logger:level()) -> unicode:chardata().
format_level(Level) ->
  atom_to_binary(Level).

-spec format_domain([atom()]) -> binary().
format_domain(Domain) ->
  format_atom_list(Domain).

-spec format_event([atom()]) -> binary().
format_event(Event) ->
  format_atom_list(Event).

-spec format_atom_list([atom()]) -> binary().
format_atom_list(Atoms) ->
  Parts = lists:map(fun erlang:atom_to_binary/1, Atoms),
  iolist_to_binary(lists:join($., Parts)).

-spec format_metadata_value(term()) -> unicode:chardata().
format_metadata_value(V) when is_binary(V) ->
  V;
format_metadata_value(V) when is_atom(V) ->
  format_metadata_value(atom_to_binary(V));
format_metadata_value(V) when is_list(V) ->
  try
    unicode:characters_to_binary(V)
  of
    String when is_binary(String) ->
      String;
    _ ->
      format_term(V)
  catch
    error:badarg ->
      format_term(V)
  end;
format_metadata_value(V) ->
  format_term(V).

-spec format_term(term()) -> unicode:chardata().
format_term(Term) ->
  Data = io_lib:format(<<"~0tp">>, [Term]),
  unicode:characters_to_binary(Data).

-spec format_string(io:format(), [term()], config()) -> iodata().
format_string(FormatString, Args, Config) ->
  PrintWidth = maps:get(print_width, Config, 160),
  MaxDepth = maps:get(max_depth, Config, 10),
  Format = io_lib:scan_format(FormatString, Args),
  Format2 = reformat(Format, PrintWidth, MaxDepth, []),
  io_lib:build_text(Format2).

-spec reformat(io:format(), pos_integer(), pos_integer(), io:format()) ->
        io:format().
reformat([Spec = #{control_char := $w} | Rest], W, D, Acc) ->
  reformat([Spec#{control_char => $W} | Rest], W, D, Acc);
reformat([Spec = #{control_char := $W, args := Args} | Rest], W, D, Acc) ->
  reformat(Rest, W, D, [Spec#{width => W, args => Args ++ [D]} | Acc]);
reformat([Spec = #{control_char := $p} | Rest], W, D, Acc) ->
  reformat([Spec#{control_char => $P} | Rest], W, D, Acc);
reformat([Spec = #{control_char := $P, args := Args} | Rest], W, D, Acc) ->
  reformat(Rest, W, D, [Spec#{width => W, args => Args ++ [D]} | Acc]);
reformat([C | Rest], W, D, Acc) ->
  reformat(Rest, W, D, [C | Acc]);
reformat([], _, _, Acc) ->
  lists:reverse(Acc).

