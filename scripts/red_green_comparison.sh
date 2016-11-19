#!/bin/bash
#Script that takes a data folder as a command line argument and creates the comparison gif for that folder
set -e
perl scripts/extractor.pl $1 Green triangle
perl scripts/extractor.pl $1 Red triangle
perl scripts/extractor.pl $1 triangle
./scripts/visualise_povray_script.sh $1/extracted_Green_triangle
./scripts/visualise_povray_script.sh $1/extracted_Red_triangle
./scripts/visualise_povray_script.sh $1/extracted_triangle
mkdir $1/red_green_comparison
for i in $(ls $1/extracted_Green_triangle); do
  montage -geometry 600x -tile 3x1 $1/extracted_triangle/$i $1/extracted_Green_triangle/$i $1/extracted_Red_triangle/$i $1/red_green_comparison/$i
done
#convert -delay 20 -loop 0 red_green_comparison/*.jpg red_green_comparison/red_green_comparison.gif
rm -r $1/extracted_Green_triangle
rm -r $1/extracted_Red_triangle
rm -r $1/extracted_triangle
