!CK Revell, February 2016

module scem_2_output_povray

  use scem_0_arrays
  use scem_0_input
  use scem_0_useful
  use scem_1_types

  implicit none

  contains

    subroutine scem_output_povray

      integer :: n

      !Create filename for povray output file.
      character(len=30)	:: povray_filename
      write(povray_filename,"(A24,I2.2,A4)") "data/povray_data/povray_", n_snapshots, ".pov"

      !Open file for povray output
      open(unit=42, file=povray_filename,status='unknown')

      write(42,*) '#version 3.5;'
      write(42,*) '#include "colors.inc"'
      write(42,*) '#include "textures.inc"'
      write(42,*) 'background {White}'
      write(42,*)
      write(42,*) 'camera {'
      write(42,*) '   location  <0, 125, 125>'
      write(42,*) '   angle 12'
      write(42,*) '   look_at<0,0,0>}'
      write(42,*)
      write(42,*) 'light_source { < -60, 60, 0 > color White }'
      write(42,*) 'light_source { < 60, -60, 0 > color White }'
      write(42,*) 'light_source { < 0, 0, 60 > color White }'
      write(42,*) 'light_source { < 0, 0, -60 > color White }'
      write(42,*)

      !Draw spheres for all elements of all cells in the system, coloured according to element type
      do i=1, 1
        do j=1, cells(i)%c_elements(0)
          k=cells(i)%c_elements(j)
          if ((elements(k)%type).EQ.1) then
            write(42,'(A12,F18.14,A2,F18.14,A2,F18.14,A51,I2.2)') ' sphere {  < ', &
                          elements(k)%position(1), ',', elements(k)%position(2), &
                            ',', elements(k)%position(3), &
                              '> 1.5 texture { pigment { color Green } } } // cell ',&
                                elements(k)%parent
          else
            write(42,'(A12,F18.14,A2,F18.14,A2,F18.14,A51,I2.2)') ' sphere {  < ', &
                          elements(k)%position(1), ',', elements(k)%position(2), &
                            ',', elements(k)%position(3), &
                              '> 1.5 texture { pigment { color Red } } } // cell ',&
                                elements(k)%parent
          endif
          write(42,*)
        enddo
      enddo

      !Draw cylinders for inter-element pair interactions if flag_povray_pairs=1
      if(flag_povray_pairs.EQ.1) then
        do j=1, np
          !Inter-cell interactions in black
          if(elements(pairs(j,1))%parent.NE.elements(pairs(j,2))%parent) then
            write(42,'(A14,F18.14,A2,F18.14,A2,F18.14,A4,F18.14,A2,F18.14,A2,&
                          F18.14,A51,I2.2,A8,I2.2)') &
                          ' cylinder {  < ', &
                          elements(pairs(j,1))%position(1), ',', &
                          elements(pairs(j,1))%position(2), ',', &
                          elements(pairs(j,1))%position(3), '>, <', &
                          elements(pairs(j,2))%position(1), ',', &
                          elements(pairs(j,2))%position(2), ',', &
                          elements(pairs(j,2))%position(3), &
                          '> 0.5 texture { pigment { color Black } } } // cell ',&
                          elements(pairs(j,1))%parent, ' , cell ',&
                          elements(pairs(j,2))%parent
            write(42,*)
          !Intra-cell cortex pair interactions in red
          elseif((elements(pairs(j,1))%type.EQ.2).AND.(elements(pairs(j,2))%type.EQ.2)) then
            write(42,'(A14,F18.14,A2,F18.14,A2,F18.14,A4,F18.14,A2,F18.14,A2,&
                          F18.14,A50,I2.2,A8,I2.2)') &
                          ' cylinder {  < ', &
                          elements(pairs(j,1))%position(1), ',', &
                          elements(pairs(j,1))%position(2), ',', &
                          elements(pairs(j,1))%position(3), '>, <', &
                          elements(pairs(j,2))%position(1), ',', &
                          elements(pairs(j,2))%position(2), ',', &
                          elements(pairs(j,2))%position(3), &
                          '> 0.5 texture { pigment { color Red } } } // cell ',&
                          elements(pairs(j,1))%parent, ' , cell ',&
                          elements(pairs(j,2))%parent
            write(42,*)
          !All other intra-cell interactions in blue
          else
            write(42,'(A14,F18.14,A2,F18.14,A2,F18.14,A4,F18.14,A2,F18.14,A2,&
                          F18.14,A50,I2.2,A8,I2.2)') &
                          ' cylinder {  < ', &
                          elements(pairs(j,1))%position(1), ',', &
                          elements(pairs(j,1))%position(2), ',', &
                          elements(pairs(j,1))%position(3), '>, <', &
                          elements(pairs(j,2))%position(1), ',', &
                          elements(pairs(j,2))%position(2), ',', &
                          elements(pairs(j,2))%position(3), &
                          '> 0.5 texture { pigment { color Blue } } } // cell ',&
                          elements(pairs(j,1))%parent, ' , cell ',&
                          elements(pairs(j,2))%parent
            write(42,*)
          endif
        enddo
      endif

      close(unit=42)

    end subroutine scem_output_povray

end module scem_2_output_povray
