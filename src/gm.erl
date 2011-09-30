%%% gm
%%%
%%% Functions for interacting with GraphicsMagick

-module(gm).
-export([
          identify_explicit/2,
          identify/2,
          convert/3,
          mogrify/2
       ]).

%% =====================================================
%% API
%% =====================================================

%% Explicit Identify
%%
%% Get explicit image characteristics in a list to be parsed by proplists:get_value
%%
%% Example:
%%
%%    identify_explicit("my.jpg", [filename, width, height, type]}).
%%
%% Which returns a list of characteristics to be retrived with proplists:get_value
%%
identify_explicit(File, Options) ->
  Template = "identify -format ':format_string' :file",
  TemplateOpts = [{file, File}, {format_string, identify_format_string(Options)}],
  Result = os:cmd(cmd_string(Template, TemplateOpts)),
  case cmd_error(Result) of
    {error, Msg}  -> {error, Msg};
    no_error      -> parse_identify_explicit(Result)
  end.

%% Identify
identify(File, Options) ->
  Template = "identify {{options}} :file",
  TemplateOpts = [{file, File}],
  exec_cmd(Template, TemplateOpts, Options).

%% Convert
convert(File, Converted, Options) ->
  Template = "convert {{options}} :input_file :output_file",
  TemplateOpts = [{input_file, File}, {output_file, Converted}],
  exec_cmd(Template, TemplateOpts, Options).

%% Mogrify
mogrify(File, Options) ->
  Template = "mogrify {{options}} :file",
  TemplateOpts = [{file, File}],
  exec_cmd(Template, TemplateOpts, Options).

%% =====================================================
%% INTERNAL FUNCTIONS
%% =====================================================

%% Run an os:cmd based on a template and passed in options
exec_cmd(Template, ExtraOptions, Options) ->
  OptString = opt_cmd_string(Options),
  PreParsed = cmd_string(Template, ExtraOptions),
  CmdString = re:replace(PreParsed, "{{options}}", OptString, [{return, list}]),
  Cmd = os:cmd("gm " ++ CmdString),
  parse_result(Cmd).

%% Create a format string from the passed in options
identify_format_string(Options) ->
  Parts = [kv_string(Option) || Option <- Options],
  Str = string:join(Parts, "--SEP--"),
  Str.

%% Parse the result of the identify command using "explicit"
parse_identify_explicit(Str) ->
  Stripped = re:replace(Str, "\n", "", [{return, list}]),
  FormatParts = re:split(Stripped, "--SEP--", [{return, list}]),
  ParsedParts = [part_to_tuple(X) || X <- FormatParts],
  ParsedParts.

%% Create a k:v format string to simplify parsing
kv_string(Option) ->
  string:join([atom_to_list(Option), gm_format_char:val(Option)], ": ").

%% Convert an identify -format response to a list of k/v pairs
part_to_tuple(X) ->
  [K,V] = re:split(X, ": ", [{return, list}]),
  K1 = list_to_atom(K),
  {K1, converted_value(K1, V)}.

%% Conversions for passed options
converted_value(width, V) ->
  list_to_integer(V);
converted_value(height, V) ->
  list_to_integer(V);
converted_value(_Label, V) ->
  V.


%% Build the option part of the command string from a list of options
opt_cmd_string(Options) ->
  opt_cmd_string("", Options).
opt_cmd_string(Str, []) ->
  Str;
opt_cmd_string(CmdStr, [Option|Options]) ->
  [OptCmdString, Opts] = gm_options:opt(Option),
  ParsedCommandStr = cmd_string(OptCmdString, Opts),
  NewCmdStr = string:join([CmdStr, ParsedCommandStr], " "),
  opt_cmd_string(NewCmdStr, Options).

%% Build a command string from a template and a list of options
cmd_string(Cmd, [{Key, Value}|Rest]) ->
  Search = ":" ++ atom_to_list(Key),
  NewCmd = re:replace(Cmd, Search, Value, [{return, list}]),
  cmd_string(NewCmd, Rest);
cmd_string(Cmd, []) ->
  Cmd.


%% Parse an error coming from an executed os:cmd
cmd_error(Cmd) ->
  R1 = re:run(Cmd, "command not found"),
  R2 = re:run(Cmd, "No such file"),
  R3 = re:run(Cmd, "Request did not return an image"),
  case R1 of
    {match, _} -> {error, command_not_found};
    _ ->
      case R2 of
        {match, _} -> {error, file_not_found};
        _ ->
          case R3 of
            {match, _} -> {error, no_image_returned};
            _ ->
              no_error
          end
      end
  end.
    
%% Return ok if successful, otherwise return a useful error
parse_result(Result) ->
  case cmd_error(Result) of
    {error, Msg} ->
      {error, Msg};
    no_error ->
      case Result of
        [] ->
          ok;
        Msg ->
          {error, Msg}
      end
  end.


%% =====================================================
%% UNIT TESTS
%% =====================================================

-ifdef(TEST).
-include_lib("eunit/include/eunit.hrl").
identify_test_() ->
  [{"Gets explicit image info", fun test_image_info/0}].

test_image_info() ->
  Img = "sandbox/cyberbrain.jpg",
  Info = identify_explicit(Img, [width]),
  ?assertMatch(600, proplists:get_value(width, Info)).

-endif.
