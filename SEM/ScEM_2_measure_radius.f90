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
		integer :: fate_count2
		real*8	:: cell_distance_squared
		real*8	:: distance_sum1
		real*8	:: distance_sum2
		real*8	:: distance_average1
		real*8	:: distance_average2
		real*8	:: average_difference
		real*8, dimension(3) :: system_COM		!Vector position of system centre of mass
		real*8, dimension(3) :: cell_vector		!Vector position of cell relative to system centre of mass


		open(unit=36,file=output_folder//'/system_data/sorting_data_radius.txt', status='unknown')

		!For now we will assume (0,0,0) is the centre of the cell aggregate.
		!Therefore no need to identify centre of mass of aggregate.
		!This allows us to calculate distances of cells from centre of
		!aggregate as just the dot product of their position vectors. Otherwise
		!would need to subtract position of cell aggregate.

		fate_count1=0
		fate_count2=0
		cell_distance_squared=0
		distance_sum1=0
		distance_sum2=0
		distance_average1=0
		distance_average2=0

		!Need to start by calculating the centre of mass of the system, which can change after each iteration due to cell division/movement.
		system_COM(:)=0
		do i=1, nc
			system_COM(:)=system_COM(:)+cells(i)%position(:)*cells(i)%c_elements(0) !For all cells sum position of cell COM multiplied by number of elements in cell (ie cell "mass")
		end do
		system_COM(:)=system_COM(:)/ne  !Centre of mass found by dividing sum by total mass, ie total number of elements.


		do n=1, nc
			cell_vector(:)			= cells(n)%position(:)-system_COM(:)						!Vector from system COM to cell
			cell_distance_squared	= cell_vector(1)**2+cell_vector(2)**2+cell_vector(3)**2		!Magnitude of vector, squared
			if(cells(n)%fate.EQ.1) then
				fate_count1		= fate_count1+1													!Increment counter for cells of fate 1 by 1
				distance_sum1	= distance_sum1 + SQRT(cell_distance_squared)					!Increase distance counter for cells of type 1 by magnitude of vector position of cell relative to system COM
			else
				if (cells(n)%fate.EQ.2) then
					fate_count2		= fate_count2+1												!Increment counter for cells of fate 2 by 1
					distance_sum2	= distance_sum2 + SQRT(cell_distance_squared)				!Increase distance counter for cells of type 2 by magnitude of vector position of cell relative to system COM
				end if
			end if
		end do

		if (fate_count1.EQ.0.OR.fate_count2.EQ.0) then
			average_difference = 0
		else
			distance_average1 = distance_sum1/fate_count1
			distance_average2 = distance_sum2/fate_count2
		endif

		average_difference = distance_average1-distance_average2

		write(36,*) real(time), average_difference
!		call flush(36)

		end subroutine scem_measure_radius

end module scem_2_measure_radius
