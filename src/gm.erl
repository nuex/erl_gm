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
  Template = "identify -format :format_string :file",
  TemplateOpts = [{file, File}, {format_string, identify_format_string(Options)}],
  Result = os:cmd(bind_data(Template, TemplateOpts, [escape])),
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
  OptString = opt_string(Options),
  PreParsed = bind_data(Template, ExtraOptions, [escape]),
  CmdString = re:replace(PreParsed, "{{options}}", OptString, [{return, list}]),
  Cmd = os:cmd(lists:concat(["gm ",CmdString])),
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
opt_string(Options) ->
  opt_string("", Options).
opt_string(OptString, []) ->
  OptString;
opt_string(OptString, [Option|Options]) ->
  NewOptString = case gm_options:opt(Option) of
    {Switch, Template, Data} ->
      Parsed = lists:concat(["'",bind_data(Template, Data, []),"'"]),
      string:join([OptString, Switch, Parsed], " ");
    {Switch} ->
      string:join([OptString, Switch], " ")
  end,
  opt_string(NewOptString, Options).

%% Bind data to a command template
bind_data(Template, [{Key, Value}|Rest], Options) ->
  Search = lists:concat([":",atom_to_list(Key)]),
  Replace = case Options of
    [escape] ->
      lists:concat(["'",Value,"'"]);
    _ ->
      Value
  end,
  NewTemplate = re:replace(Template, Search, Replace, [{return, list}]),
  bind_data(NewTemplate, Rest, Options);
bind_data(Template, [], _Options) ->
  Template.


%% Parse an error coming from an executed os:cmd
cmd_error(Cmd) ->
  Errors = [
    {"command not found", command_not_found},
    {"No such file", file_not_found},
    {"Request did not return an image", no_image_returned},
    {"unable to open image", unable_to_open}
  ],
  parse_error(Cmd, Errors).

%% Run through each error, checking for a match.
%% Return `no_error` when there are no more possibilities.
parse_error(_, []) ->
  no_error;
parse_error(Cmd, [{ErrorDescription, Error}|Errors]) ->
  case re:run(Cmd, ErrorDescription) of
    {match, _} -> {error, Error};
    _ ->
      parse_error(Cmd, Errors)
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
gm_test_() ->
  [
    {"Returns a file_not_found error", fun test_file_not_found/0},
    {"Gets explicit image info", fun test_image_info/0},
    {"Doesn't get hacked", fun test_escapes_hacking/0}
  ].

test_file_not_found() ->
  ?assertMatch({error, file_not_found}, identify("doesntexist.jpg", [])).

test_image_info() ->
  Img = "sandbox/cyberbrain.jpg",
  Info = identify_explicit(Img, [width]),
  ?assertMatch(600, proplists:get_value(width, Info)).

test_escapes_hacking() ->
  mogrify("baz", [{output_directory, "$(touch hackingz)"}]),
  ?assertMatch(false, filelib:is_file("hackingz")).

-endif.
