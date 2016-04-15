!CK Revell, February 2016

module scem_2_output_povray

  use scem_0_arrays
  use scem_0_input
  use scem_0_useful
  use scem_1_types

  implicit none

  contains

    subroutine scem_output_povray

      character(len=48)	:: povray_filename
      real*8  :: sphere_radius  !Radius of sphere used to represent cell volume in povray visualiation. Calculated from cell volume.

      !Create filename for povray output file.
      write(povray_filename,"(A20,I2.2,A4)") "/povray_data/povray_", n_snapshots, ".pov"

      !Open file for povray output
      open(unit=42, file=output_folder//povray_filename,status='unknown')

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
      write(42,*) 'light_source { < -60, 60, 0 > color White }'
      write(42,*) 'light_source { < 60, -60, 0 > color White }'
      write(42,*) 'light_source { < 0, 0, 60 > color White }'
      write(42,*) 'light_source { < 0, 0, -60 > color White }'
      write(42,*)

      if (flag_povray_elements.EQ.1) then
        !Draw spheres for all elements of all cells in the system, coloured according to element type
        do i=1, ne
          if ((elements(i)%type).EQ.1) then
            write(42,'(A12,F18.14,A2,F18.14,A2,F18.14,A61,I2.2)') ' sphere {  < ',elements(i)%position(1), ',', elements(i)%position(2),',', elements(i)%position(3),'> 1.5 texture { pigment { color Green } } } // element, cell ',elements(i)%parent
          else
            write(42,'(A12,F18.14,A2,F18.14,A2,F18.14,A59,I2.2)') ' sphere {  < ',elements(i)%position(1), ',', elements(i)%position(2),',', elements(i)%position(3),'> 1.5 texture { pigment { color Red } } } // element, cell ',elements(i)%parent
          endif
        enddo
      endif
      write(42,*)

      if (flag_povray_pairs.EQ.1) then
        !Draw cylinders for inter-element pair interactions. Inter-cell pairs black, intra-cell pairs blue.
        do j=1, np
          m = pairs(j,1)
          n = pairs(j,2)
          k = elements(m)%parent
          l = elements(n)%parent

          if (k.NE.l) then !Elements are not in the same cell
            !Inter-cell interactions in black
            write(42,'(A14,F18.14,A2,F18.14,A2,F18.14,A4,F18.14,A2,F18.14,A2,&
                          F18.14,A63,I2.2,A8,I2.2)') &
                          ' cylinder {  < ', &
                          elements(m)%position(1), ',', &
                          elements(m)%position(2), ',', &
                          elements(m)%position(3), '>, <', &
                          elements(n)%position(1), ',', &
                          elements(n)%position(2), ',', &
                          elements(n)%position(3), &
                          '> 0.5 texture { pigment { color Black } } } // pair inter cell ',&
                          elements(m)%parent, ' , cell ',&
                          elements(n)%parent


            !Intra-cell cortex pair interactions in red
  !          elseif((elements(pairs(j,1))%type.EQ.2).AND.(elements(pairs(j,2))%type.EQ.2)) then
  !            write(42,'(A14,F18.14,A2,F18.14,A2,F18.14,A4,F18.14,A2,F18.14,A2,&
  !                          F18.14,A50,I2.2,A8,I2.2)') &
  !                          ' cylinder {  < ', &
  !                          elements(pairs(j,1))%position(1), ',', &
  !                          elements(pairs(j,1))%position(2), ',', &
  !                          elements(pairs(j,1))%position(3), '>, <', &
  !                          elements(pairs(j,2))%position(1), ',', &
  !                          elements(pairs(j,2))%position(2), ',', &
  !                          elements(pairs(j,2))%position(3), &
  !                          '> 0.5 texture { pigment { color Red } } } // cell ',&
  !                          elements(pairs(j,1))%parent, ' , cell ',&
  !                          elements(pairs(j,2))%parent
          else !Elements are in the same cell
            !All intra-cell interactions in blue
            write(42,'(A14,F18.14,A2,F18.14,A2,F18.14,A4,F18.14,A2,F18.14,A2,&
                            F18.14,A62,I2.2,A8,I2.2)') &
                            ' cylinder {  < ', &
                            elements(pairs(j,1))%position(1), ',', &
                            elements(pairs(j,1))%position(2), ',', &
                            elements(pairs(j,1))%position(3), '>, <', &
                            elements(pairs(j,2))%position(1), ',', &
                            elements(pairs(j,2))%position(2), ',', &
                            elements(pairs(j,2))%position(3), &
                            '> 0.5 texture { pigment { color Blue } } } // pair intra cell ',&
                            elements(pairs(j,1))%parent, ' , cell ',&
                            elements(pairs(j,2))%parent
          endif
        enddo
      endif
      write(42,*)

      !Write cell position data to file in povray format
      if (flag_povray_cells.EQ.1) then
        !Draw spheres for all cells in the system, coloured according to cell type, with transparency set to 0.66 and phong set to 0.8
        do i=1, nc
          sphere_radius = (3.0*cells(i)%volume/(pi*4.0))**(1.0/3.0)     !Radius is cube root of (3*volume/4pi)
          if ((cells(i)%fate).EQ.1) then
            write(42,'(A12,F18.14,A2,F18.14,A2,F18.14,A2,F18.14,A82,I2.2)') &
                  ' sphere {  < ', cells(i)%position(1), ',', cells(i)%position(2), &
                  ',', cells(i)%position(3), '> ', sphere_radius,' texture { pigment { color Green transmit .66}finish{phong .8} } } // volume cell ', cells(i)%label
          else
            write(42,'(A12,F18.14,A2,F18.14,A2,F18.14,A2,F18.14,A80,I2.2)') &
                  ' sphere {  < ', cells(i)%position(1), ',', cells(i)%position(2), &
                  ',', cells(i)%position(3), '> ', sphere_radius,' texture { pigment { color Red transmit .66}finish{phong .8} } } // volume cell ', cells(i)%label
          endif
          write(42,*)
        enddo
      endif

      close(unit=42)

    end subroutine scem_output_povray

end module scem_2_output_povray
