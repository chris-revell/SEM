!CK Revell February 2016

module scem_2_output_gnuplot

  use scem_0_arrays
  use scem_0_input
  use scem_0_useful
  use scem_1_types

  implicit none

  contains

  subroutine scem_output_gnuplot

      !Need to tidy up the comments in this file. 

      character(len=1024)	:: cells_filename_epi
      character(len=1024)	:: cells_filename_hypo
      character(len=1024)	:: elements_filename

      ! write data to files in gnuplot-voronoi format.
      ! A file is created for each cell at each timestep, with each element in a cell represented by a voronoi element.
      ! These files are drawn in gnuplot together so that an image is created for each timestep containing all elements of all cells, with each cell or tissue type having a different colour.

      ! Two files are created at each timestep containing the positions of all cells of each cell type, again represented as voronoi elements. These can be drawn separately from the element files to show the positions of cell centre of masses at each timestep and get a different perspective on relative movement of cells.

      if (mod(time,(time_out_1)).lt.dt) then           !time_out_1 = cell_cycle_time/10.0 i.e. the time interval between graphical data outputs

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

      end if

    end subroutine scem_output_gnuplot

end module scem_2_output_gnuplot
