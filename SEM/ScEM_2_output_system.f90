! T. J Newman, Tempe, July 2010

module scem_2_output_system

  use scem_0_arrays
  use scem_0_input
  use scem_0_useful
  use scem_1_types
  use scem_2_measure_interface
  use scem_2_measure_radius
  use scem_2_measure_neighbours

  implicit none

    contains

    subroutine scem_output_system

        !Calculate time expired so far
        call SYSTEM_CLOCK(current_time)
        total_system_time = (current_time-start_time)/count_rate

        !Write system progress update to the command line.
        write(*,*) real(time),total_system_time,ne,nc,np,n_snapshots !Old version: real(time),ne,nc,ne_size,nc_size,np,np_size,nx,ny,nz,n_snapshots

        !Print time and cell count to cell_count file to allow cell count to be plotted against time
        if (flag_count_output.EQ.1) then
          open(unit=37,file=output_folder//'/system_data/cell_count.txt', status='unknown')
      	  write(37,*) real(time), nc
        endif

        !Write cell fate data at each snapshot to file
        if (flag_fate_output.EQ.1) then
          open(unit=26,file=output_folder//'/system_data/cell_fate_data_final.txt', status='unknown')
          do n=1, nc
            write(26,*) cells(n)%fate
          end do
          close(unit=26)
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
        if (flag_measure_interface.EQ.1) then
          call scem_measure_interface
        endif

        if (flag_measure_radius.EQ.1) then
          call scem_measure_radius
        endif

        if (flag_measure_neighbours.EQ.1) then
          call scem_measure_neighbours
        endif


    end subroutine scem_output_system

end module scem_2_output_system
