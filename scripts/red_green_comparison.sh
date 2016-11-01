#Script that takes a data folder as a command line argument and creates the comparison gif for that folder

perl scripts/extractor.pl $1 Green triangle
perl scripts/extractor.pl $1 Red triangle
perl scripts/extractor.pl $1 triangle
cd $1/extracted_Green_triangle
./visualise_povray_script.sh
cd ../extracted_Red_triangle
./visualise_povray_script.sh
cd ../extracted_triangle
./visualise_povray_script.sh
cd ..
mkdir red_green_comparison
for i in $(ls extracted_Green_triangle); do
  montage -geometry 600x -tile 3x1 extracted_triangle/$i extracted_Green_triangle/$i extracted_Red_triangle/$i red_green_comparison/$i
done
convert -delay 20 -loop 0 red_green_comparison/*.jpg red_green_comparison/red_green_comparison.gif
