# erl_gm

An Erlang GraphicsMagick wrapper

## USAGE

    % Crop image into 100x100 tiles
    gm:convert("/some/image.jpg", "tile", [{crop, 100, 100}]).

    % Get multiple identify properties (returns a list of key value pairs
    % to be parsed by proplist:get_value
    gm:identify_explicit("/some/image.jpg", [width, height, filename, type]).

    % Crazytown
    gm:convert("/some/image.jpg", "/something/crazy.jpg", [
      flip,
      magnify,
      {rotate, 45},
      {blur, 7, 3},
      {crop, 300, 300, 150, 130},
      {edge, 3}
    ]).

    % Resize
    gm:convert("/some/image.jpg", "/something/resized.jpg", [{resize, 240, 240}]).

## AVAILABLE GM FUNCTIONS

    gm:composite
    gm:convert
    gm:identify
    gm:mogrify
    gm:montage
    gm:version

## AVAILABLE GM OPTIONS

    -adjoin
    -blur
    -create_directories
    -crop
    -define
    -dissolve
    -edge
    -extent
    -flip
    -format
    -gravity
    -interlace
    -magnify
    -negate
    -output_directory
    -quality
    -resize
    -rotate
    -size
    -thumbnail
    -type
    -watermark

## AVAILABLE SHORTCUTS

  gm:identify_explicit

## EXTENDING

New options can be added to `gm_options.erl` to broaden option support. Just open a pull request and I'll merge in additions.

## LICENSE

MIT
