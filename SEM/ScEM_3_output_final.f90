! T. J Newman, Tempe, July 2010

module scem_3_output_final

  use scem_0_arrays
  use scem_0_input
  use scem_1_types
  use scem_2_output_povray

  implicit none

  contains

    subroutine scem_output_final

      integer :: i,n

      !Calculate total time of run. Print to command line and end_of_run_data
      call SYSTEM_CLOCK(current_time)
      total_system_time = (current_time-start_time)/count_rate
      write(*,*) "Time taken for run:", total_system_time/60, "minutes"

      ! write final system data to files
      if (flag_elements_final.EQ.1) then
        open(unit=24,file=output_folder//'/system_data/elements_final.txt',status='unknown')
        do i=1,ne
          write(24,'(3f12.6,i4)')elements(i)%position(:),elements(i)%parent
        end do
      endif

      !Write cell fate data to file
      if (flag_fate_output.EQ.1) then
        open(unit=26,file=output_folder//'/system_data/cell_fate_data_final.txt', status='unknown')
        do n=1, nc
          write(26,*) cells(n)%fate
        end do
        close(unit=26)
      endif

      open(unit=25,file=output_folder//'/system_data/end_of_run_data.txt',status='unknown')
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

    end subroutine scem_output_final

end module scem_3_output_final
