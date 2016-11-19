# Run povray to visualise elements and/or pairs in 100 snapshots of the system.
#Then run ImageMagick to create an animated gif of the output jpegs.

set -e #Exit script if any of the povray renderings fail.

#Run povray on all data files
for i in $(ls $1); do
  if [ ${i: -4:4} == ".pov" ]
  then
    povray $1/$i +Fj -D +H900 +W1200 +o$1${i:0: -4}".pov"
  fi
done

#ImageMagick command line input
for i in $(ls $1); do
  if [ ${i: -4:4} == ".jpg" ]
  then
    convert $1/$i -shave 150x0 $i
  fi
done

#convert -delay 15 -loop 0 $1/*.jpg $1/animated.gif
#rm $1/*.jpg
rm $1/*.pov
