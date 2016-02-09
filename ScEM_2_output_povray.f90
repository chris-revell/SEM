! T. J Newman, Tempe, July 2010

module scem_2_output_final

  use scem_0_arrays
  use scem_0_input
  use scem_0_useful
  use scem_1_types

  implicit none

  contains

    subroutine scem_output_final

      ! write data to files
      open(unit=24,file='data/system_data/elements_final',status='unknown')
      open(unit=25,file='data/system_data/end_of_run_data',status='unknown')
      do i=1,ne
         write(24,'(3f12.6,i4)')elements(i)%position(:),elements(i)%parent
      end do
      write(25,*)nc
      write(25,*)ne
      write(25,*)np
      write(25,*)'number of cells = ',nc
      write(25,*)'number of elements = ',ne
      write(25,*)'number of element pairs = ',np
      write(25,*)'time = ',real(time)
      write(25,*)'com = ',real(cells(1)%position(:))

      close(unit=24)
      close(unit=25)

      !Povray experimental output
      !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
      open(unit=42,file='data/system_data/elements_povray.pov',status='unknown')
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

      do n=1, ne
        if ((elements(n)%type).EQ.1) then
          write(*,*) elements(n)%type, 'green'
          write(42,'(A12,F18.14,A2,F18.14,A2,F18.14,A43)') ' sphere {  < ', &
                        elements(n)%position(1), ',', elements(n)%position(2), &
                          ',', elements(n)%position(3), &
                            '> 1.5 texture { pigment { color Green } } }'
        else
          write(*,*) elements(n)%type, 'red'
          write(42,'(A12,F18.14,A2,F18.14,A2,F18.14,A43)') ' sphere {  < ', &
                        elements(n)%position(1), ',', elements(n)%position(2), &
                          ',', elements(n)%position(3), &
                            '> 1.5 texture { pigment { color Red } } }'
        endif
        write(42,*)
      enddo
      close(unit=42)
      !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!


    end subroutine scem_output_final

end module scem_2_output_final
