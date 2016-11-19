#Script to run all components of simulation.
#Compile latest updates to program
./compile_script.sh
#If a compilation fails the compile script is set to exit and return a non-zero status.
#If this happens we remove all .o and .mod files that may have been produced and
#exit the run script without running ScEM_master
if [[ $? -ne 0 ]] ; then
    rm *.o
    rm *.mod
    exit 1
fi
#Run program
nohup caffeinate -id ./ScEM_master 0.3 3.0
