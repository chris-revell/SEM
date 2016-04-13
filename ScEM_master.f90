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

  implicit none

  integer :: start_time
  integer :: end_time
  integer :: total_system_time
  real    :: count_rate

  !Take time when run is initiated
  call SYSTEM_CLOCK(start_time)

  ! Retrieve system parameters and constants. scem_input is a subroutine in ScEM_0_input.f90 that initialises a large number of system parameters that have already been defined but not evaluated. To vary the system parameters simply change the numbers in this subroutine.
  call scem_input

  !Create labelled file for data output
  !Catch date and time, create folder to store data in
  call date_and_time(DATE=date_of_run,TIME=time_of_run)
  output_folder = "data/"//date_of_run//"_"//time_of_run
  call system("mkdir "//output_folder)
  call system("mkdir "//output_folder//"/system_data")
  call system("mkdir "//output_folder//"/povray_pairs_data")
  call system("mkdir "//output_folder//"/povray_elements_data")
  call system("mkdir "//output_folder//"/povray_cells_data")

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

  !Take time when run finishes
  call SYSTEM_CLOCK(end_time,count_rate)
  !Calculate total time of run and print to command line.
  total_system_time = (end_time-start_time)/count_rate
  write(*,*) "Time taken for run (seconds):", total_system_time

end program scem_master
