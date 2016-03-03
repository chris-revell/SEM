! T. J Newman, Tempe, July 2010

module scem_2_output_final

  use scem_0_arrays
  use scem_0_input
  use scem_0_useful
  use scem_1_types
  use scem_1_plotting_commands

  implicit none

  contains

    subroutine scem_output_final

      ! write final system data to files
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

      call scem_plotting_commands

    end subroutine scem_output_final

end module scem_2_output_final
