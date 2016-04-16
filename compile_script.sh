# script to precompile ScEM modules, compile ScEM master program, and then delete .mod and .o files.
# Note that the first line must be a command to move to the directory in which the code sits. This is necessary on a Mac but not for Linux.

gfortran -c SEM/ScEM_0*.f90 -ffree-line-length-none
gfortran -c SEM/ScEM_1*.f90 -ffree-line-length-none
gfortran -c SEM/ScEM_2*.f90 -ffree-line-length-none
gfortran -c SEM/lstptr_module.f90 -ffree-line-length-none
gfortran -c SEM/store_module.f90 -ffree-line-length-none
gfortran -c SEM/jrand_module.f90 -ffree-line-length-none
gfortran -c SEM/swptst_module.f90 -ffree-line-length-none
gfortran -c SEM/insert_module.f90 -ffree-line-length-none
gfortran -c SEM/bdyadd_module.f90 -ffree-line-length-none
gfortran -c SEM/covsph_module.f90 -ffree-line-length-none
gfortran -c SEM/intadd_module.f90 -ffree-line-length-none
gfortran -c SEM/swap_module.f90 -ffree-line-length-none
gfortran -c SEM/trfind_module.f90 -ffree-line-length-none
gfortran -c SEM/addnod_module.f90 -ffree-line-length-none
gfortran -c SEM/left_module.f90 -ffree-line-length-none
gfortran -c SEM/trmesh_module.f90 -ffree-line-length-none
gfortran -c SEM/trlist2_module.f90 -ffree-line-length-none
gfortran -c SEM/volume_calculate_module.f90 -ffree-line-length-none
gfortran -c SEM/volume_conserve_module.f90 -ffree-line-length-none
gfortran -c SEM/ScEM_3*.f90 -ffree-line-length-none
gfortran -c SEM/ScEM_4*.f90 -ffree-line-length-none
gfortran -c SEM/ScEM_5*.f90 -ffree-line-length-none
gfortran SEM/swptst_module.f90 SEM/lstptr_module.f90 SEM/store_module.f90 SEM/jrand_module.f90 SEM/insert_module.f90 SEM/bdyadd_module.f90 SEM/covsph_module.f90 SEM/intadd_module.f90 SEM/swap_module.f90 SEM/trfind_module.f90 SEM/addnod_module.f90 SEM/left_module.f90 SEM/trmesh_module.f90 SEM/trlist2_module.f90 SEM/ScEM_0*.f90 SEM/ScEM_1*.f90 SEM/ScEM_2*.f90 SEM/volume_calculate_module.f90 SEM/volume_conserve_module.f90 SEM/ScEM_3*.f90 SEM/ScEM_4*.f90 SEM/ScEM_5*.f90 SEM/ScEM_master.f90 SEM/rng.f90 -o ScEM_master -ffree-line-length-none
rm *.mod *.o
