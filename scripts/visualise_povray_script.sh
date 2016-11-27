# Run povray to visualise elements and/or pairs in 100 snapshots of the system.
#Then run ImageMagick to create an animated gif of the output jpegs.

set -e #Exit script if any of the povray renderings fail.

#Run povray on all data files
for i in $(ls $1); do
  if [ ${i: -4:4} == ".pov" ]
  then
    povray $1/$i +FJ Output_File_Name=$1/${i:0:7}".jpg" -D +H900 +W1200 >/dev/null
  fi
done

#ImageMagick command line input
for i in $(ls $1); do
  if [ ${i: -4:4} == ".jpg" ]
  then
    convert $1/$i -shave 150x0 $1/$i
  fi
done

#convert -delay 15 -loop 0 $1/*.jpg $1/animated.gif
#rm $1/*.jpg
rm $1/*.pov
