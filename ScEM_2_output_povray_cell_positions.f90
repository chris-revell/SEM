!CK Revell, February 2016

module scem_2_output_povray_cell_positions

  use scem_0_arrays
  use scem_0_input
  use scem_0_useful
  use scem_1_types
  use cube_root_module

  implicit none

  contains

    subroutine scem_output_povray_cell_positions

      integer :: n
      real*8  :: sphere_radius  !Radius of sphere used to represent cell in povray visualiation. Calculated from cell volume.

      !Create filename for povray output file.
      character(len=40)	:: povray_cells_filename
      write(povray_cells_filename,"(A34,I2.2,A4)") "data/povray_cell_&
                                          data/povray_cell_", n_snapshots, ".pov"

      !Open file for povray output
      open(unit=43, file=povray_cells_filename,status='unknown')

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
        sphere_radius = cube_root(3.0*cells(i)%volume/(pi*4.0))     !Radius is cube root of (3*volume/4pi)
        if ((cells(i)%fate).EQ.1) then
          write(43,'(A12,F18.14,A2,F18.14,A2,F18.14,A2,F18.14,A47,I2.2)') &
                ' sphere {  < ', cells(i)%position(1), ',', cells(i)%position(2), &
                ',', cells(i)%position(3), '> ', sphere_radius,' texture { pigment &
                { color Green } } } // cell ', cells(i)%label
        else
          write(43,'(A12,F18.14,A2,F18.14,A2,F18.14,A2,F18.14,A45,I2.2)') &
                ' sphere {  < ', cells(i)%position(1), ',', cells(i)%position(2), &
                ',', cells(i)%position(3), '> ', sphere_radius,' texture { pigment &
                { color Red } } } // cell ', cells(i)%label
        endif
        write(43,*)
      enddo

      close(unit=43)

    end subroutine scem_output_povray_cell_positions

end module scem_2_output_povray_cell_positions
