! T. J Newman, Tempe, July 2010

module scem_2_output

  use scem_0_arrays
  use scem_0_input
  use scem_0_useful
  use scem_1_types
  use scem_2_measure
  use scem_2_measure_radius
  use scem_2_measure_neighbours

  implicit none

  contains

  subroutine scem_output

    character(len=1024)	:: cells_filename_epi
    character(len=1024)	:: cells_filename_hypo
    character(len=1024)	:: elements_filename

      ! write data to files
      if (mod(time,(time_out_1)).lt.dt) then                          !time_out_1 = cell_cycle_time/10.0 i.e. the time interval between graphical data outputs

         open(unit=29,file='data/system_data/snapshot_data.txt', status='unknown')
         open(unit=37,file='data/system_data/cell_count.txt', status='unknown')

         n_snapshots=n_snapshots+1
         do n=1,ne
        	write(21,'(3f12.6,i4)')elements(n)%position(:),elements(n)%type		!21 is file "elements". 3f12.6,i4 means print 3 float variables consisting of 12 characters (11 digits and a point), with 6 digits after the point, then on the same line, one integer with 4 digits. So this is printing all 3 components of position plus one "parent" integer.
         end do
         write(21,*)
         write(21,*)

         write(*,*) real(time),ne,nc,ne_size,nc_size,np,np_size,nx,ny,nz,n_snapshots    !This is the line which prints data to the command line
         write(29,*) nc													   				!Prints the number of cells in each data snapshot to the file "snapshot_data"
		 call flush(29)
      	 call scem_measure																!Measure numerical sorting value of system
      	 call scem_measure_radius
      	 call scem_measure_neighbours

      	 write(37,*) real(time), nc												!Prints time and cell count to cell_count file to allow cell count to be plotted against time

      	 !For this snapshot, create a new output filename for cell positions and open the file
      	 write(cells_filename_epi,"(A26,I2.2)") "data/cell_data/cells_epi_S", n_snapshots
		 write(cells_filename_hypo,"(A27,I2.2)") "data/cell_data/cells_hypo_S", n_snapshots
		 open(unit=19,file=trim(cells_filename_epi),status='unknown')
		 open(unit=20,file=trim(cells_filename_hypo),status='unknown')

      	 !The following do loop creates a data file in the voro++ input format for every cell at every visualisation snapshot. Also creates gnuplot command files for each snapshot that take the form fort.n_snapshots
      	 do k=1, nc
        	!For each cell in this snapshot, create a new output filename for element positions and open the file
        	write(elements_filename,"(A28,I2.2,A2,I2.2)") "data/element_data/elements_S", n_snapshots, "_C", k
        	open(unit=18,file=trim(elements_filename),status='unknown')
        	!Now write element labels and positions to the file (voro++ format)
         	do l=1, cells(k)%c_elements(0)
         		n=cells(k)%c_elements(l)
         		write(18,*) n, elements(n)%position(:)
         	end do
         	close(18)

         	!Write cell label and positions to cell output file in voro++ format
         	if (cells(k)%fate.EQ.1) then
		 		write(19,*) cells(k)%label, cells(k)%position(:)
		 	else
		 		write(20,*) cells(k)%label, cells(k)%position(:)
		 	end if
		 end do
		 close(19)
		 close(20)

		 !Write cell fate data to file
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

      end if

    end subroutine scem_output

end module scem_2_output
