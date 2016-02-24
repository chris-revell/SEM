!CK Revell, February 2016

module scem_2_output_povray_cell_positions

  use scem_0_arrays
  use scem_0_input
  use scem_0_useful
  use scem_1_types

  implicit none

  contains

    subroutine scem_output_povray_cell_positions

      integer :: n

      !Create filename for povray output file.
      character(len=35)	:: povray_cells_filename
      write(povray_filename,"(A29,I2.2,A4)") "data/povray_cell_data/povray_", n_snapshots, ".pov"

      !Open file for povray output
      open(unit=39, file=povray_cells_filename,status='unknown')

      write(42,*) '#version 3.5;'
      write(42,*) '#include "colors.inc"'
      write(42,*) '#include "textures.inc"'
      write(42,*) 'background {White}'
      write(42,*)
      write(42,*) 'camera {'
      write(42,*) '   location  <500, 0, 0>'
      write(42,*) '   angle 12'
      write(42,*) '   look_at<0,0,0>}'
      write(42,*)
      write(42,*) 'light_source { < -600, 600, 0 > color White }'
      write(42,*) 'light_source { < 600, -600, 0 > color White }'
      write(42,*) 'light_source { < 0, 0, 600 > color White }'
      write(42,*) 'light_source { < 0, 0, -600 > color White }'
      write(42,*)

      !Draw spheres for all cells in the system, coloured according to cell type
      do i=1, nc
        if ((cells(i)%fate).EQ.1) then
          write(39,'(A12,F18.14,A2,F18.14,A2,F18.14,A51,I2.2)') ' sphere {  < ', &
                        cells(i)%position(1), ',', cells(i)%position(2), &
                          ',', cells(i)%position(3), &
                            '> 10 texture { pigment { color Green } } } // cell ',&
                              cells(i)%label
        else
          write(39,'(A12,F18.14,A2,F18.14,A2,F18.14,A51,I2.2)') ' sphere {  < ', &
                        cells(i)%position(1), ',', cells(i)%position(2), &
                          ',', cells(i)%position(3), &
                            '> 10 texture { pigment { color Red } } } // cell ',&
                              cells(i)%label
        endif
        write(39,*)
      enddo

      close(unit=42)

    end subroutine scem_output_povray_cell_positions

end module scem_2_output_povray_cell_positions
