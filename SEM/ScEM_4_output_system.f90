! T. J Newman, Tempe, July 2010

module scem_4_output_system

  use scem_0_arrays
  use scem_0_input
  use scem_0_useful
  use scem_1_types
  use scem_2_measure_radius
  use scem_2_measure_neighbours
  use scem_2_measure_displacement
  use scem_2_measure_type_radius
  use scem_2_measure_surface
  use scem_2_measure_velocity
  use scem_3_measure_randomised

  implicit none

    contains

    subroutine scem_output_system

        !Calculate time expired so far
        call SYSTEM_CLOCK(current_time)
        total_system_time = (current_time-start_time)/count_rate

        !Write system progress update to the command line.
        write(*,*) real(time),total_system_time,ne,nc,np,n_snapshots

        !Print time and cell count to cell_count file to allow cell count to be plotted against time
        if (flag_count_output.EQ.1) then
          open(unit=28,file=output_folder//'/system_data/cell_count.txt', status='unknown')
      	  write(28,*) real(time), nc
        endif

        !Write cell volume data to file
        if (flag_volume_output.EQ.1) then
          open(unit=27,file=output_folder//'/system_data/cell_volumes.txt',status='unknown',position="append")
          write(27,'(F24.12,A)',advance="no") time, "	"
          do n=1, nc-1
            write(27,'(F24.12,A)',advance="no") cells(n)%volume, "	"
          end do
          write(27,'(F24.12,A)',advance='yes') cells(nc)%volume		!Advance line only after the last volume value so that all volumes appear on the same line.
          close(unit=27)
        endif

        !Sorting measurements
        if (flag_measure_radius.EQ.1)       call scem_measure_radius

        if (flag_measure_neighbours.EQ.1)   call scem_measure_neighbours

        if (flag_measure_displacement.EQ.1) call scem_measure_displacement

        if (flag_measure_type_radius.EQ.1)  call scem_measure_type_radius

        if (flag_measure_surface.EQ.1)      call scem_measure_surface

        if (flag_measure_randomised.EQ.1)   call scem_measure_randomised

        if (flag_measure_velocity.EQ.1)     call scem_measure_velocity

    end subroutine scem_output_system

end module scem_4_output_system
