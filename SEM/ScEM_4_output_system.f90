! T. J Newman, Tempe, July 2010

module scem_4_output_system

  use scem_0_arrays
  use scem_0_input
  use scem_1_types
  use scem_2_measure_radius
  use scem_2_measure_neighbours
  use scem_2_measure_displacement
  use scem_2_measure_surface
  use scem_2_measure_velocity
  use scem_2_measure_com
  use scem_3_measure_randomised

  implicit none

    contains

    subroutine scem_output_system

      integer :: n

      !Print time and cell count to cell_count file to allow cell count to be plotted against time
      if (flag_count_output.EQ.1) then
        open(unit=28,file=output_folder//'/system_data/cell_count.txt', status='unknown')
        write(28,*) real(time), epicellcount, (nc-epicellcount) !epicellcount calculated in scem_com
        close(28)
      endif

      !Write cell volume data to file
      if (flag_volume_output.EQ.1) then
        open(unit=27,file=output_folder//'/system_data/cell_volumes.txt',status='unknown',position="append")
        do n=1, nc
          write(27,*) time, cells(n)%label, cells(n)%fate, cells(n)%volume, cells(n)%surfaceArea
        end do
        close(27)
      endif

      !Sorting measurements
      if (flag_measure_radius.EQ.1)       call scem_measure_radius
      if (flag_measure_neighbours.EQ.1)   call scem_measure_neighbours
      if (flag_measure_surface.EQ.1)      call scem_measure_surface
      if (flag_measure_randomised.EQ.1)   call scem_measure_randomised
      if (flag_measure_displacement.EQ.1) call scem_measure_displacement
      if (flag_measure_velocity.EQ.1)     call scem_measure_velocity
      if (flag_measure_com.EQ.1)          call scem_measure_com

    end subroutine scem_output_system

end module scem_4_output_system
