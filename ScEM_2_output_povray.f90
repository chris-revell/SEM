!CK Revell, February 2016

module scem_2_output_povray

  use scem_0_arrays
  use scem_0_input
  use scem_0_useful
  use scem_1_types

  implicit none

  contains

    subroutine scem_output_povray

      integer ::  n

      !Create filename for povray output file.
      character(len=30)	:: povray_filename
      write(povray_filename,"(A24,I2,A4)") "data/povray_data/povray_", n_snapshots, ".pov"

      !Open file for povray output
      open(unit=42, file=povray_filename,status='unknown')

      do i=1, nc

        write(42,*) '#version 3.5;'
        write(42,*) '#include "colors.inc"'
        write(42,*) '#include "textures.inc"'
        write(42,*) 'background {White}'
        write(42,*)
        write(42,*) 'camera {'
        write(42,*) '   location  <0, 80, -200>'
        write(42,*) '   angle 12'
        write(42,*) '   look_at<0,-0.5,0>}'
        write(42,*)
        write(42,*) 'light_source { < -30, 30, 0 > color White }'
        write(42,*)

        !Draw spheres for all elements in the system, coloured according to element type
        do j=1, cells(i)%c_elements(0)
          !n is the global label of the jth element in c_elements
          n=cells(i)%c_elements(j)
          if ((elements(n)%type).EQ.1) then
            write(42,'(A12,F18.14,A2,F18.14,A2,F18.14,A43)') ' sphere {  < ', &
                          elements(n)%position(1), ',', elements(n)%position(2), &
                            ',', elements(n)%position(3), &
                              '> 1.5 texture { pigment { color Green } } }'
          else
            write(42,'(A12,F18.14,A2,F18.14,A2,F18.14,A43)') ' sphere {  < ', &
                          elements(n)%position(1), ',', elements(n)%position(2), &
                            ',', elements(n)%position(3), &
                              '> 1.5 texture { pigment { color Red } } }'
          endif
          write(42,*)
        enddo

        !Draw cylinders for inter-element pair interactions
        do j=1, np
          write(42,'(A14,F18.14,A2,F18.14,A2,F18.14,A4,F18.14,A2,F18.14,A2,&
                        F18.14,A42)') &
                        ' cylinder {  < ', &
                        elements(n)%position(1), ',', elements(n)%position(2), &
                        ',', elements(n)%position(3), '>, <', &
                        elements(n)%position(1), ',', elements(n)%position(2), &
                        ',', elements(n)%position(3), &
                        '> 0.5 texture { pigment { color Blue } } }'
          write(42,*)
        enddo

    enddo

    close(unit=42)

    end subroutine scem_output_povray

end module scem_2_output_povray
