!C.K.Revell, November 2013
!ScEM_2_measure_2
!For a spherical ball of cells, we expect sorting to occur radially, so that
!one type of cells moves to the outside and the other moves to the inside.
!With this module we use distance of cells from the centre of mass of the aggregate
!to measure sorting.

module scem_2_measure_radius

	use scem_0_input
	use scem_0_arrays
	use scem_0_useful
	use scem_1_types

	implicit none

	contains

		subroutine scem_measure_radius

		integer :: fate_count1
		real*8	:: cell_distance_squared
		real*8	:: cell_distance
		real*8	:: distance_sum1
		real*8	:: distance_average1
		real*8	:: normalised_radius
		real*8	:: max_cell_radius
		real*8, dimension(3) :: system_COM		!Vector position of system centre of mass
		real*8, dimension(3) :: cell_vector		!Vector position of cell relative to system centre of mass


		open(unit=28,file=output_folder//'/sorting_data/sorting_data_radius.txt', status='unknown')

		!For now we will assume (0,0,0) is the centre of the cell aggregate.
		!Therefore no need to identify centre of mass of aggregate.
		!This allows us to calculate distances of cells from centre of
		!aggregate as just the dot product of their position vectors. Otherwise
		!would need to subtract position of cell aggregate.

		fate_count1=0
		cell_distance_squared=0
		distance_sum1=0
		distance_average1=0
		max_cell_radius=0

		!Need to start by calculating the centre of mass of the system, which can change after each iteration due to cell division/movement.
		system_COM(:)=0
		do i=1, ne
			system_COM(:)=system_COM(:)+elements(i)%position(:)
		enddo
		system_COM(:)=system_COM(:)/ne  !Centre of mass found by dividing sum by total mass, ie total number of elements.


		do n=1, nc
			!For each cell, calculate distance from the system centre of mass
			cell_vector(:)				= cells(n)%position(:)-system_COM(:)											!Vector from system COM to cell
			cell_distance_squared	= cell_vector(1)**2+cell_vector(2)**2+cell_vector(3)**2		!Magnitude of vector, squared
			cell_distance					= SQRT(cell_distance_squared)
			!If the radius of this cell exceeds the current max_cell_radius, update max_cell_radius
			max_cell_radius				= MAX(max_cell_radius, cell_distance)

			!If the cell is of type 1 (epiblast), add its distance to the sum and increment the count
			if(cells(n)%fate.EQ.1) then
				fate_count1		= fate_count1+1													!Increment counter for cells of fate 1 by 1
				distance_sum1	= distance_sum1 + cell_distance					!Increase distance counter for cells of type 1 by magnitude of vector position of cell relative to system COM
			end if
		end do

		!Divide the distance sum for cell type 1 by the cell count, and normalise by the maximum cell radius
		!Terms included to prevent division by 0 in the case of 0 cells of type 1.
		if (fate_count1.EQ.0) then
			normalised_radius = 1
		elseif (max_cell_radius.EQ.0) then
			normalised_radius = 0
		else
			distance_average1 = distance_sum1/fate_count1
			normalised_radius = distance_average1/max_cell_radius
		endif

		write(28,*) real(time), normalised_radius

		end subroutine scem_measure_radius

end module scem_2_measure_radius
