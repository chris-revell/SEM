!CK Revell, February 2016

module scem_2_output_povray_pairs

  use scem_0_arrays
  use scem_0_input
  use scem_0_useful
  use scem_1_types

  implicit none

  contains

    subroutine scem_output_povray_pairs

      integer :: k
      integer :: l

      !Create filename for povray output file.
      character(len=42)	:: povray_filename
      write(povray_filename,"(A32,I2.2,A4)") "/povray_pairs_data/povray_pairs_", n_snapshots, ".pov"

      !Open file for povray output
      open(unit=44, file=output_folder//povray_filename,status='unknown')

      write(44,*) '#version 3.5;'
      write(44,*) '#include "colors.inc"'
      write(44,*) '#include "textures.inc"'
      write(44,*) 'background {White}'
      write(44,*)
      write(44,*) 'camera {'
      write(44,*) '   location  <500, 0, 0>'
      write(44,*) '   angle 12'
      write(44,*) '   look_at<0,0,0>}'
      write(44,*)
      write(44,*) 'light_source { < -60, 60, 0 > color White }'
      write(44,*) 'light_source { < 60, -60, 0 > color White }'
      write(44,*) 'light_source { < 0, 0, 60 > color White }'
      write(44,*) 'light_source { < 0, 0, -60 > color White }'
      write(44,*)

      !Draw cylinders for inter-element pair interactions
      !Draw on a cell by cell basis to separate data by cell spatially in output file for ease of manipulation later on

      do i=1, nc
        write(44,'(A7,I2.2)') "//Cell ", i
        do j=1, np
          !For all pairs, check if either of the two elements in the pair are in cell i before drawing.
          !This means that intra-element pairs will be drawn twice but other than increasing file size
          !this shouldn't affect the output too much. Can always change later if it does.
          k = elements(pairs(j,1))%parent
          l = elements(pairs(j,2))%parent
          if (k.EQ.i.OR.l.EQ.i) then
            !Inter-cell interactions in black
            if(k.NE.l) then
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
            endif
          endif
        enddo
        write(44,*)
      enddo

      close(unit=44)

    end subroutine scem_output_povray_pairs

end module scem_2_output_povray_pairs
