-module(gm_options).
-export([opt/1]).


opt({crop, Width, Height}) ->
  ["-crop", ":widthx:height", [
    {width, integer_to_list(Width)},
    {height, integer_to_list(Height)}
  ]];
opt({output_directory, Dir}) ->
  ["-output-directory", ":output_directory", [{output_directory, Dir}]];
opt(create_directories) ->
  ["-create-directories", "", []];
opt(flip) ->
  ["-flip", "", []];
opt(magnify) ->
  ["-magnify", "", []];
opt({rotate, Degrees}) ->
  ["-rotate", ":degrees", [{degrees, integer_to_list(Degrees)}]];
opt({blur, Radius, Sigma}) ->
  ["-blur", ":radiusx:sigma", [
    {radius, integer_to_list(Radius)},
    {sigma, integer_to_list(Sigma)}
  ]];
opt({crop, Width, Height, XOffset, YOffset}) ->
  ["-crop", ":widthx:height+:x_offset+:y_offset", [
    {width, integer_to_list(Width)},
    {height, integer_to_list(Height)},
    {x_offset, integer_to_list(XOffset)},
    {y_offset, integer_to_list(YOffset)}
  ]];
opt({edge, Radius}) ->
  ["-edge", ":radius", [{radius, integer_to_list(Radius)}]].
