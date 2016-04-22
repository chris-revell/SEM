! T. J Newman, Tempe, July 2010

module scem_2_initial_exist

  use scem_0_arrays
  use scem_0_input
  use scem_0_useful
  use scem_1_types

  implicit none

  contains

    subroutine scem_initial_exist

      ! initialize position, label, and age of first cell
      do n=1, nc
!  	    cells(n)%position(:)=0.0 ! this is the approximate position of first cell - not its center of mass
      	cells(n)%label=n
      	cells(n)%age=establishment_time
	  	cells(n)%fate=read_cell_fate(n)
	  end do

      ! open data file of element positions
      open(unit=12,file='config_files/elements_config.txt',status='old')
      ! read element positions from file
      do n=1,ne
         read(12,*)elements(n)%position(:), elements(n)%parent
         elements(n)%label=n ! define new element label
         elements(n)%type=2 ! define new element type
         elements(n)%stage=1 ! define status of element to be steady-state
         elements(n)%age=establishment_time ! initialize age of new element
         elements(n)%strength=1.0 ! weighting for potential interactions
         elements(n)%velocity(:)=0.0 ! initialize velocity of new element
      end do

      ! close elements data file
      close(unit=12)

      ! shift element positions relative to pre-defined position of first cell
!      do n=1,ne
!         elements(n)%position(:)=cells(1)%position(:)+elements(n)%position(:)
!      end do

    end subroutine scem_initial_exist

end module scem_2_initial_exist
