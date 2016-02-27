! T. J Newman, Tempe, July 2010

module scem_2_output_system

  use scem_0_arrays
  use scem_0_input
  use scem_0_useful
  use scem_1_types
  use scem_2_measure
  use scem_2_measure_radius
  use scem_2_measure_neighbours

  implicit none

    contains

    subroutine scem_output_system

!***********************
!Do we really need to write all elements at all time points to the same file?
!        do n=1,ne
!          write(21,'(3f12.6,i4)')elements(n)%position(:),elements(n)%type		!21 is file "elements". 3f12.6,i4 means print 3 float variables consisting of 12 characters (11 digits and a point), with 6 digits after the point, then on the same line, one integer with 4 digits. So this is printing all 3 components of position plus one "parent" integer.
!        end do
!        write(21,*)
!        write(21,*)
!**********************

        !Write system progress update to the command line.
        write(*,*) real(time),ne,nc,ne_size,nc_size,np,np_size,nx,ny,nz,n_snapshots

        !Print the number of cells in each data snapshot (time point) to the file "snapshot_data"
        open(unit=29,file='data/system_data/snapshot_data.txt', status='unknown')
        write(29,*) nc
		    call flush(29)

        !Print time and cell count to cell_count file to allow cell count to be plotted against time
        open(unit=37,file='data/system_data/cell_count.txt', status='unknown')
      	write(37,*) real(time), nc

        !Write cell fate data at each snapshot to file
        open(unit=26,file='data/system_data/cell_fate_data_final', status='unknown')
        do n=1, nc
          write(26,*) cells(n)%fate
        end do
        close(unit=26)

		    !Write cell volume data to file
        open(unit=27,file='data/system_data/cell_volumes',status='unknown',position="append")
        write(27,'(F24.12,A)',advance="no") time, "	"
        do n=1, nc-1
          write(27,'(F24.12,A)',advance="no") cells(n)%volume, "	"
        end do
        write(27,'(F24.12,A)',advance='yes') cells(nc)%volume		!Advance line only after the last volume value so that all volumes appear on the same line.
        close(unit=27)


    end subroutine scem_output_system

end module scem_2_output_system
