! T. J. Newman, Tempe, July 2010

! extended to include growth, September 2010
! unnecessary features removed

program scem_master

  use scem_0_input
  use scem_1_inflexion
  use scem_1_potential
  use scem_2_deallocate
  use scem_2_output_final
  use scem_5_initialize
  use scem_5_iterate
  use omp_lib

  implicit none

  write(*,*) "Number of processors: ", omp_get_num_procs()

  ! Retrieve system parameters and constants. scem_input is a subroutine in ScEM_0_input.f90 that initialises a large number of system parameters that have already been defined but not evaluated. To vary the system parameters simply change the numbers in this subroutine.
  call scem_input

  ! Construct interaction potential tables and locate inflexion point(s). scem_potential and scem_inflexion are subroutines contained within ScEM_1_potential.f90 and ScEM_1_inflexion.f90.
  call scem_potential
  call scem_inflexion

  ! initialize system and create/input first cell
  call scem_initialize

  ! iterate system
  call scem_iterate

  ! output final data files
  call scem_output_final

  ! deallocate arrays
  call scem_deallocate

end program scem_master
