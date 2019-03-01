# script to precompile ScEM modules, compile ScEM master program, and then delete .mod and .o files.

set -e #Exit script if any compile command fails and returns a non-zero status.
echo "Compiling..."
ifort -c -O3 SEM/ScEM_0*.f90
ifort -c -O3 SEM/ScEM_1*.f90
ifort -c -O3 SEM/ScEM_2*.f90
ifort -c -O3 SEM/lstptr_module.f90
ifort -c -O3 SEM/store_module.f90
ifort -c -O3 SEM/jrand_module.f90
ifort -c -O3 SEM/swptst_module.f90
ifort -c -O3 SEM/insert_module.f90
ifort -c -O3 SEM/bdyadd_module.f90
ifort -c -O3 SEM/covsph_module.f90
ifort -c -O3 SEM/intadd_module.f90
ifort -c -O3 SEM/swap_module.f90
ifort -c -O3 SEM/trfind_module.f90
ifort -c -O3 SEM/addnod_module.f90
ifort -c -O3 SEM/left_module.f90
ifort -c -O3 SEM/trmesh_module.f90
ifort -c -O3 SEM/trlist2_module.f90
ifort -c -O3 SEM/ScEM_3*.f90
ifort -c -O3 SEM/ScEM_4*.f90
ifort -c -O3 SEM/ScEM_5*.f90
echo "Linking..."
ifort -O3 SEM/swptst_module.f90 SEM/lstptr_module.f90 SEM/store_module.f90 SEM/jrand_module.f90 SEM/insert_module.f90 SEM/bdyadd_module.f90 SEM/covsph_module.f90 SEM/intadd_module.f90 SEM/swap_module.f90 SEM/trfind_module.f90 SEM/addnod_module.f90 SEM/left_module.f90 SEM/trmesh_module.f90 SEM/trlist2_module.f90 SEM/ScEM_0*.f90 SEM/ScEM_1*.f90 SEM/ScEM_2*.f90 SEM/ScEM_3*.f90 SEM/ScEM_4*.f90 SEM/ScEM_5*.f90 SEM/ScEM_master.f90 -o ScEM_master
rm *.mod *.o
