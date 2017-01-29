!Module to calculate the radius of each preblast cell from the system centre of mass, normliased by the maximum radius of any cell in the system.
!C.K.Revell, November 2013, Updated October 2016

module scem_2_measure_radius

	use scem_0_input
	use scem_0_arrays
	use scem_1_types

	implicit none

contains

	subroutine scem_measure_radius

		integer :: i,precount
		real*8	:: meanradius
		real*8, dimension(3) :: pre_COM		!Vector position of system centre of mass
		real*8, dimension(3) :: cell_vector		!Vector position of cell relative to system centre of mass

		if (randomising) then
			open(unit=35,file=output_folder//'/randomised_data/radius.txt',status='unknown',position="append")
		else
			open(unit=35,file=output_folder//'/sorting_data/radius.txt', status='unknown',position="append")
		endif

		!Need to start by calculating the centre of mass of the system, which can change after each iteration due to cell movement.
		pre_COM(:)= 0
		precount  = 0
		do i=1, ne
			if (cells(elements(i)%parent)%fate.EQ.2) then
				pre_COM(:)=pre_COM(:)+elements(i)%position(:)
				precount = precount + 1
			endif
		enddo
		pre_COM(:)=pre_COM(:)/precount  !Centre of mass found by dividing sum by total mass, ie total number of elements.

		precount = 0
		meanradius = 0
		do i=1, nc
			if (cells(i)%fate.EQ.2) then
				cell_vector = cells(i)%position - pre_COM
				meanradius = meanradius + SQRT(DOT_PRODUCT(cell_vector,cell_vector))
				precount = precount + 1
			endif
		enddo
		meanradius = meanradius/precount

		write(35,*) time, meanradius

		close(35)

	end subroutine scem_measure_radius

end module scem_2_measure_radius
