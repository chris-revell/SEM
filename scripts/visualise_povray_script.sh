# Run povray to visualise elements and/or pairs in 100 snapshots of the system.
#Then run ImageMagick to create an animated gif of the output jpegs.

set -e #Exit script if any of the povray renderings fail.

#Run povray on all data files
for i in $(ls); do
  if [ ${i:0:1} == "s"]
  then
    povray snap_00.pov +Fj -D +H900 +W1200
  fi
done

#ImageMagick command line input
for i in $(ls); do
  if [ ${i: -4:4} == ".jpg" ]
  then
    convert $i -shave 150x0 $i
  fi
done

#convert -delay 15 -loop 0 *.jpg animated.gif
#rm *.jpg
rm *.pov
rm visualise_povray_script.sh
