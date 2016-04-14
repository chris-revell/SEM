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
      real*8  :: sphere_radius  !Radius of sphere used to represent cell in povray visualiation. Calculated from cell volume.

      !Create filename for povray output file.
      character(len=42)	:: povray_cells_filename
      write(povray_cells_filename,"(A32,I2.2,A4)") "/povray_cells_&
                                          data/povray_cells_", n_snapshots, ".pov"

      !Open file for povray output
      open(unit=43, file=output_folder//povray_cells_filename,status='unknown')

      write(43,*) '#version 3.5;'
      write(43,*) '#include "colors.inc"'
      write(43,*) '#include "textures.inc"'
      write(43,*) 'background {White}'
      write(43,*)
      write(43,*) 'camera {'
      write(43,*) '   location  <500, 0, 0>'
      write(43,*) '   angle 12'
      write(43,*) '   look_at<0,0,0>}'
      write(43,*)
      write(43,*) 'light_source { < -600, 600, 0 > color White }'
      write(43,*) 'light_source { < 600, -600, 0 > color White }'
      write(43,*) 'light_source { < 0, 0, 600 > color White }'
      write(43,*) 'light_source { < 0, 0, -600 > color White }'
      write(43,*)

      !Draw spheres for all cells in the system, coloured according to cell type
      do i=1, nc
        sphere_radius = (3.0*cells(i)%volume/(pi*4.0))**(1.0/3.0)     !Radius is cube root of (3*volume/4pi)
        if ((cells(i)%fate).EQ.1) then
          write(43,'(A12,F18.14,A2,F18.14,A2,F18.14,A2,F18.14,A75,I2.2)') &
                ' sphere {  < ', cells(i)%position(1), ',', cells(i)%position(2), &
                ',', cells(i)%position(3), '> ', sphere_radius,' texture { pigment &
                { color Green transmit .66}finish{phong .8} } } // cell ', cells(i)%label
        else
          write(43,'(A12,F18.14,A2,F18.14,A2,F18.14,A2,F18.14,A73,I2.2)') &
                ' sphere {  < ', cells(i)%position(1), ',', cells(i)%position(2), &
                ',', cells(i)%position(3), '> ', sphere_radius,' texture { pigment &
                { color Red transmit .66}finish{phong .8} } } // cell ', cells(i)%label
        endif
        write(43,*)
      enddo

      close(unit=43)

    end subroutine scem_output_povray_cell_positions

end module scem_2_output_povray_cell_positions
