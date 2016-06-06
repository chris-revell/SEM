# script to precompile ScEM modules, compile ScEM master program, and then delete .mod and .o files.

set -e #Exit script if any compile command fails and returns a non-zero status.
ifort -c -g -traceback -check all -fpe0 -warn -nogen-interface -debug full SEM/ScEM_0*.f90
ifort -c -g -traceback -check all -fpe0 -warn -nogen-interface -debug full SEM/ScEM_1*.f90
ifort -c -g -traceback -check all -fpe0 -warn -nogen-interface -debug full SEM/ScEM_2*.f90
ifort -c -g -traceback -check all -fpe0 -warn -nogen-interface -debug full SEM/lstptr_module.f90
ifort -c -g -traceback -check all -fpe0 -warn -nogen-interface -debug full SEM/store_module.f90
ifort -c -g -traceback -check all -fpe0 -warn -nogen-interface -debug full SEM/jrand_module.f90
ifort -c -g -traceback -check all -fpe0 -warn -nogen-interface -debug full SEM/swptst_module.f90
ifort -c -g -traceback -check all -fpe0 -warn -nogen-interface -debug full SEM/insert_module.f90
ifort -c -g -traceback -check all -fpe0 -warn -nogen-interface -debug full SEM/bdyadd_module.f90
ifort -c -g -traceback -check all -fpe0 -warn -nogen-interface -debug full SEM/covsph_module.f90
ifort -c -g -traceback -check all -fpe0 -warn -nogen-interface -debug full SEM/intadd_module.f90
ifort -c -g -traceback -check all -fpe0 -warn -nogen-interface -debug full SEM/swap_module.f90
ifort -c -g -traceback -check all -fpe0 -warn -nogen-interface -debug full SEM/trfind_module.f90
ifort -c -g -traceback -check all -fpe0 -warn -nogen-interface -debug full SEM/addnod_module.f90
ifort -c -g -traceback -check all -fpe0 -warn -nogen-interface -debug full SEM/left_module.f90
ifort -c -g -traceback -check all -fpe0 -warn -nogen-interface -debug full SEM/trmesh_module.f90
ifort -c -g -traceback -check all -fpe0 -warn -nogen-interface -debug full SEM/trlist2_module.f90
ifort -c -g -traceback -check all -fpe0 -warn -nogen-interface -debug full SEM/volume_calculate_module.f90
ifort -c -g -traceback -check all -fpe0 -warn -nogen-interface -debug full SEM/volume_conserve_module.f90
#ifort -c -g -traceback -check all -fpe0 -warn -nogen-interface -debug full SEM/ScEM_3*.f90
ifort -c -g -traceback -check all -fpe0 -warn -nogen-interface -debug full SEM/ScEM_4*.f90
ifort -c -g -traceback -check all -fpe0 -warn -nogen-interface -debug full SEM/ScEM_5*.f90
ifort -g -traceback -check all -fpe0 -warn -nogen-interface -debug full SEM/swptst_module.f90 SEM/lstptr_module.f90 SEM/store_module.f90 SEM/jrand_module.f90 SEM/insert_module.f90 SEM/bdyadd_module.f90 SEM/covsph_module.f90 SEM/intadd_module.f90 SEM/swap_module.f90 SEM/trfind_module.f90 SEM/addnod_module.f90 SEM/left_module.f90 SEM/trmesh_module.f90 SEM/trlist2_module.f90 SEM/ScEM_0*.f90 SEM/ScEM_1*.f90 SEM/ScEM_2*.f90 SEM/volume_calculate_module.f90 SEM/volume_conserve_module.f90 SEM/ScEM_4*.f90 SEM/ScEM_5*.f90 SEM/ScEM_master.f90 -o ScEM_master #SEM/ScEM_3*.f90 SEM/ScEM_4*.f90 SEM/ScEM_5*.f90 SEM/ScEM_master.f90 -o ScEM_master

rm *.mod *.o
