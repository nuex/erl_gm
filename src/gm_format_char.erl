-module(gm_format_char).
-export([val/1]).

val(file_size) -> "%b";
val(comment) -> "%c";
val(directory) -> "%d";
val(filename_extension) -> "%e";
val(filename) -> "%f";
val(height) -> "%h";
val(input_filename) -> "%i";
val(number_of_unique_colors) -> "%k";
val(label) -> "%l";
val(type) -> "%m"; % type is "magick" in graphicsmagick
val(output_filename) -> "%o";
val(page_number) -> "%p";
val(image_minimum_bit_depth) -> "%q";
val(image_type_description) -> "%r";
val(scene_number) -> "%s";
val(top_of_filename) -> "%t";
val(unique_temporary_filename) -> "%u";
val(width) -> "%w";
val(x_resolution) -> "%x";
val(y_resolution) -> "%y";
val(signature) -> "%#".
