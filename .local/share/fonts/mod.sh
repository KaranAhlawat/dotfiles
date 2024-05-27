#!/bin/bash
dir="open-sans"
prev_name="Office Code Pro"
new_name="Leo Code"

# Create an empty array to store the file names
for file in ./$dir/*; do
	# Add the file name to the array
	IFS=\/ read -r base name font <<< $file
	IFS=- read -r name weight <<< $font
	IFS=' ' read -r -a new_font_name <<< $new_name
	IFS=; pyftfeatfreeze -R  "'$prev_name/$new_name'" $file "./mod/$new_font_name-${weight}"
done
