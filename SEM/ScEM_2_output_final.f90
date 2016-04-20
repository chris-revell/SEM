! T. J Newman, Tempe, July 2010

module scem_2_output_final

  use scem_0_arrays
  use scem_0_input
  use scem_0_useful
  use scem_1_types
  use scem_1_plotting_commands

  implicit none

  contains

    subroutine scem_output_final

      !Calculate total time of run. Print to command line and end_of_run_data
      call SYSTEM_CLOCK(current_time)
      total_system_time = (current_time-start_time)/count_rate
      write(*,*) "Time taken for run (seconds):", total_system_time

      ! write final system data to files
      open(unit=24,file=output_folder//'/system_data/elements_final',status='unknown')
      open(unit=25,file=output_folder//'/system_data/end_of_run_data',status='unknown')
      do i=1,ne
         write(24,'(3f12.6,i4)')elements(i)%position(:),elements(i)%parent
      end do
      write(25,*)nc
      write(25,*)ne
      write(25,*)np
      write(25,*)'number of cells = ',nc
      write(25,*)'number of elements = ',ne
      write(25,*)'number of element pairs = ',np
      write(25,*)'time = ',real(time)
      write(25,*)"Time taken for run (seconds):",total_system_time

      close(unit=24)
      close(unit=25)

      call scem_plotting_commands

      call system('gnuplot -c "'//output_folder//'/system_data/gnuplot_commands_system_plots.gnu"')
!      call system("cp visualise_povray_script.sh "//output_folder//"povray_data")  This line doesn't work and creates a strange executable file

      write(*,*) "Output folder is:", output_folder

    end subroutine scem_output_final

end module scem_2_output_final