# script to precompile ScEM modules, compile ScEM master program, and then delete .mod and .o files.
# Note that the first line must be a command to move to the directory in which the code sits. This is necessary on a Mac but not for Linux.

ifort -c -parallel SEM/ScEM_0*.f90
ifort -c -parallel SEM/ScEM_1*.f90
ifort -c -parallel SEM/ScEM_2*.f90
ifort -c -parallel SEM/lstptr_module.f90
ifort -c -parallel SEM/store_module.f90
ifort -c -parallel SEM/jrand_module.f90
ifort -c -parallel SEM/swptst_module.f90
ifort -c -parallel SEM/insert_module.f90
ifort -c -parallel SEM/bdyadd_module.f90
ifort -c -parallel SEM/covsph_module.f90
ifort -c -parallel SEM/intadd_module.f90
ifort -c -parallel SEM/swap_module.f90
ifort -c -parallel SEM/trfind_module.f90
ifort -c -parallel SEM/addnod_module.f90
ifort -c -parallel SEM/left_module.f90
ifort -c -parallel SEM/trmesh_module.f90
ifort -c -parallel SEM/trlist2_module.f90
ifort -c -parallel SEM/volume_calculate_module.f90
ifort -c -parallel SEM/volume_conserve_module.f90
#ifort -c -parallel SEM/ScEM_3*.f90
ifort -c -parallel SEM/ScEM_4*.f90
ifort -c -parallel SEM/ScEM_5*.f90
ifort SEM/swptst_module.f90 SEM/lstptr_module.f90 SEM/store_module.f90 SEM/jrand_module.f90 SEM/insert_module.f90 SEM/bdyadd_module.f90 SEM/covsph_module.f90 SEM/intadd_module.f90 SEM/swap_module.f90 SEM/trfind_module.f90 SEM/addnod_module.f90 SEM/left_module.f90 SEM/trmesh_module.f90 SEM/trlist2_module.f90 SEM/ScEM_0*.f90 SEM/ScEM_1*.f90 SEM/ScEM_2*.f90 SEM/volume_calculate_module.f90 SEM/volume_conserve_module.f90 SEM/ScEM_4*.f90 SEM/ScEM_5*.f90 SEM/ScEM_master.f90 SEM/rng.f90 -o ScEM_master #SEM/ScEM_3*.f90 SEM/ScEM_4*.f90 SEM/ScEM_5*.f90 SEM/ScEM_master.f90 SEM/rng.f90 -o ScEM_master

rm *.mod *.o
