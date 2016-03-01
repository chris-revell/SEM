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
      write(povray_filename,"(A36,I2.2,A4)") "data/povray_pairs_data/povray_pairs_", n_snapshots, ".pov"

      !Open file for povray output
      open(unit=44, file=povray_filename,status='unknown')

      write(44,*) '#version 3.5;'
      write(44,*) '#include "colors.inc"'
      write(44,*) '#include "textures.inc"'
      write(44,*) 'background {White}'
      write(44,*)
      write(44,*) 'camera {'
      write(44,*) '   location  <0, 125, 125>'
      write(44,*) '   angle 12'
      write(44,*) '   look_at<0,0,0>}'
      write(44,*)
      write(44,*) 'light_source { < -60, 60, 0 > color White }'
      write(44,*) 'light_source { < 60, -60, 0 > color White }'
      write(44,*) 'light_source { < 0, 0, 60 > color White }'
      write(44,*) 'light_source { < 0, 0, -60 > color White }'
      write(44,*)

      !Draw cylinders for inter-element pair interactions if flag_povray_pairs=1
      do j=1, np
        !Inter-cell interactions in black
        if(elements(pairs(j,1))%parent.NE.elements(pairs(j,2))%parent) then
          write(44,'(A14,F18.14,A2,F18.14,A2,F18.14,A4,F18.14,A2,F18.14,A2,&
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
          write(44,*)
        !Intra-cell cortex pair interactions in red
        elseif((elements(pairs(j,1))%type.EQ.2).AND.(elements(pairs(j,2))%type.EQ.2)) then
          write(44,'(A14,F18.14,A2,F18.14,A2,F18.14,A4,F18.14,A2,F18.14,A2,&
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
          write(44,*)
        !All other intra-cell interactions in blue
        else
          write(44,'(A14,F18.14,A2,F18.14,A2,F18.14,A4,F18.14,A2,F18.14,A2,&
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
          write(44,*)
        endif
      enddo

      close(unit=44)

    end subroutine scem_output_povray

end module scem_2_output_povray
