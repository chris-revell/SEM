!Module to calculate the radius of each epiblast cell from the system centre of mass, normliased by the maximum radius of any cell in the system.
!C.K.Revell, November 2013, Updated October 2016

module scem_2_measure_radius

	use scem_0_input
	use scem_0_arrays
	use scem_1_types

	implicit none

contains

	subroutine scem_measure_radius

		integer :: i,n
		real*8	:: dist
		real*8	:: normalised_radius
		real*8	:: max_cell_radius
		real*8, dimension(3) :: system_COM		!Vector position of system centre of mass
		real*8, dimension(3) :: cell_vector		!Vector position of cell relative to system centre of mass

		if (randomising) then
			open(unit=35,file=output_folder//'/randomised_data/radius.txt',status='unknown',position="append")
		else
			open(unit=35,file=output_folder//'/sorting_data/radius.txt', status='unknown',position="append")
		endif

		!Need to start by calculating the centre of mass of the system, which can change after each iteration due to cell movement.
		system_COM(:)=0
		do i=1, ne
			system_COM(:)=system_COM(:)+elements(i)%position(:)
		enddo
		system_COM(:)=system_COM(:)/ne  !Centre of mass found by dividing sum by total mass, ie total number of elements.

		!Find the max cell radius of the whole system
		max_cell_radius = 0
		do n=1, nc
			!For each cell, calculate distance from the system centre of mass
			cell_vector(:)				= cells(n)%position(:)-system_COM(:)		!Vector from system COM to cell
			dist									= SQRT(DOT_PRODUCT(cell_vector,cell_vector))	!Magnitude of vector, squared
			!If the radius of this cell exceeds the current max_cell_radius, update max_cell_radius
			max_cell_radius				= MAX(max_cell_radius, dist)
		end do

		!Normalise radii of epiblast cells with max_cell_radius and write to file.
		do n=1, nc
			if (cells(n)%fate.EQ.1) then
				!For each cell, calculate distance from the system centre of mass
				cell_vector(:)		= cells(n)%position(:)-system_COM(:)		!Vector from system COM to cell
				dist							= SQRT(DOT_PRODUCT(cell_vector,cell_vector))	!Magnitude of vector, squared
				if (max_cell_radius.GT.0) then
					normalised_radius	= dist/max_cell_radius
				else
					normalised_radius = 0
				endif
				write(35,*) time, normalised_radius
			endif
		end do

		close(35)

	end subroutine scem_measure_radius

end module scem_2_measure_radius
