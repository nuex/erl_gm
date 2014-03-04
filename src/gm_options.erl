-module(gm_options).
-export([opt/1]).


opt({crop, Width, Height}) ->
  {"-crop", ":widthx:height", [
    {width, Width},
    {height, Height}
  ]};
opt({resize, Width, Height}) ->
  {"-resize", ":widthx:height", [
    {width, Width},
    {height, Height}
  ]};
opt({output_directory, Dir}) ->
  {"-output-directory", ":output_directory", [{output_directory, Dir}]};
opt(create_directories) ->
  {"-create-directories"};
opt(flip) ->
  {"-flip"};
opt(magnify) ->
  {"-magnify"};
opt({rotate, Degrees}) ->
  {"-rotate", ":degrees", [{degrees, Degrees}]};
opt({blur, Radius, Sigma}) ->
  {"-blur", ":radiusx:sigma", [
    {radius, Radius},
    {sigma, Sigma}
  ]};
opt({crop, Width, Height, XOffset, YOffset}) ->
  {"-crop", ":widthx:height+:x_offset+:y_offset", [
    {width, Width},
    {height, Height},
    {x_offset, XOffset},
    {y_offset, YOffset}
  ]};
opt({edge, Radius}) ->
  {"-edge", ":radius", [{radius, Radius}]};
opt({size, Width, Height}) ->
  {"-size", ":widthx:height", [
    {width, Width},
    {height, Height}
  ]};
opt({thumbnail, Width, Height}) ->
  {"-thumbnail", ":widthx:height^", [
    {width, Width},
    {height, Height}
  ]};
opt({gravity, Gravity}) ->
  {"-gravity", ":gravity", [{gravity, Gravity}]};
opt({quality, Quality}) ->
  {"-quality", ":quality", [{quality, Quality}]};
opt({extent, Width, Height}) ->
  {"-extent", ":widthx:height", [
    {width, Width},
    {height, Height}
  ]};
opt({type, Type}) ->
  {"-type", ":type", [{type, Type}]};
opt({interlace, Interlace}) ->
  {"-interlace", ":interlace", [{interlace, Interlace}]};
opt({format, Format}) ->
  {"-format", ":format", [{format, Format}]};
opt(adjoin) ->
  {"-adjoin"};
opt('+adjoin') ->
  {"+adjoin"}.
