#!/bin/bash

# Define the output GIF filename
output_gif="modelsummary_gallery.gif"

# Use ImageMagick to create an animated GIF from images starting with 'tinytable_gallery_'
# -delay is increased for a slower animation (e.g., 100 equals 1 second per frame)
# -resize option is used to fit images within a 1280x720 frame (16:9 aspect ratio)
# convert -delay 250 -loop 0 tinytable_gallery_*.png -resize 1250x690 -gravity center -extent 1280x720 "$output_gif"
# convert -delay 250 -loop 0 tinytable_gallery_*.png -resize 1000x500 -gravity center -extent 1025x576 "$output_gif"
convert -delay 400 -loop 0 gallery_*.png -resize 1000x500 -gravity center -extent 1025x576 -layers Optimize -colors 128 "$output_gif"

# Display a message when done
echo "Animated GIF created: $output_gif"
