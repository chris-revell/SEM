# script to precompile ScEM modules, compile ScEM master program, and then delete .mod and .o files.

#set -e #Exit script if any compile command fails and returns a non-zero status.
options="-g -openmp -traceback -check all -fpe0 -warn -nogen-interface -debug full"
ifort -c $options SEM/ScEM_0*.f90
ifort -c $options SEM/ScEM_1*.f90
ifort -c $options SEM/ScEM_2*.f90
ifort -c $options SEM/lstptr_module.f90
ifort -c $options SEM/store_module.f90
ifort -c $options SEM/jrand_module.f90
ifort -c $options SEM/swptst_module.f90
ifort -c $options SEM/insert_module.f90
ifort -c $options SEM/bdyadd_module.f90
ifort -c $options SEM/covsph_module.f90
ifort -c $options SEM/intadd_module.f90
ifort -c $options SEM/swap_module.f90
ifort -c $options SEM/trfind_module.f90
ifort -c $options SEM/addnod_module.f90
ifort -c $options SEM/left_module.f90
ifort -c $options SEM/trmesh_module.f90
ifort -c $options SEM/trlist2_module.f90
ifort -c $options SEM/ScEM_3*.f90
ifort -c $options SEM/ScEM_4*.f90
ifort -c $options SEM/ScEM_5*.f90
ifort $options -traceback SEM/swptst_module.f90 SEM/lstptr_module.f90 SEM/store_module.f90 SEM/jrand_module.f90 SEM/insert_module.f90 SEM/bdyadd_module.f90 SEM/covsph_module.f90 SEM/intadd_module.f90 SEM/swap_module.f90 SEM/trfind_module.f90 SEM/addnod_module.f90 SEM/left_module.f90 SEM/trmesh_module.f90 SEM/trlist2_module.f90 SEM/ScEM_0*.f90 SEM/ScEM_1*.f90 SEM/ScEM_2*.f90 SEM/ScEM_3*.f90 SEM/ScEM_4*.f90 SEM/ScEM_5*.f90 SEM/ScEM_master.f90 -o ScEM_master #SEM/ScEM_3*.f90 SEM/ScEM_4*.f90 SEM/ScEM_5*.f90 SEM/ScEM_master.f90 -o ScEM_master

rm *.mod *.o
