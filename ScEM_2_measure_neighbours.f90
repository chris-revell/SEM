!C.K.Revell, March 2014

module scem_2_measure_neighbours

	use scem_0_input
	use scem_0_arrays
	use scem_0_useful
	use scem_1_types

	implicit none

	contains

		subroutine scem_measure_neighbours

		integer :: sorting_count
		integer :: dummy_parent_1
		integer :: dummy_parent_2
		integer :: parent_1
		integer :: parent_2
		integer :: neighbours_counter
		integer :: neighbours_counter_final
		integer :: same_fate_counter
		real	:: sorting_measure
		integer	:: fate_1
		integer	:: fate_2
		integer :: pair_type
		character(len=1024)		:: vectors_filename
!  		character(len=1024)		:: vectors_filename_hh
!    	character(len=1024)		:: vectors_filename_eh
    	real*8, dimension(3)	:: cell_position1
    	real*8, dimension(3)	:: cell_position2
    	real*8, dimension(6)	:: vector					!First 3 elements are x,y,z position of vector, ie halfway point between cells in neighbour pair. Next 3 elements are components of vector itself, ie x,y,z separation of the two cells in the neighbour pair


		allocate(neighbours(np,2))

		open(unit=38,file='data/sorting_data/sorting_data_neighbours.txt', status='unknown')

		neighbours_counter=0
		sorting_count=0

		!First of all need to create nearest neighbour array from pairs array.
		!neighbours(:,:) array defined in ScEM_0_arrays.f90.
		!Takes the same form as pairs(:,:) ie first argument is label of pair, then 2nd argument is 1 or 2, which give the integer labels of the first and second cell in the neighbour pair.
		!NB neighbours array allocated in scem_initialize

		!Start by looking at all element-element interaction pairs
		do n=1, np
			!Find labels of parent cells for elements in interaction pair
			dummy_parent_1 = elements(pairs(n,1))%parent
			dummy_parent_2 = elements(pairs(n,2))%parent

			if (dummy_parent_1.NE.dummy_parent_2) then

				!Ensure parent_2>parent_1 for ease of manipulation later
				if (dummy_parent_1.GT.dummy_parent_2) then
					parent_1=dummy_parent_2
					parent_2=dummy_parent_1
				else
					parent_1=dummy_parent_1
					parent_2=dummy_parent_2
				end if

				!Following procedure avoids including A-B pair more than once.
				!Only need to do this once there are other components in neighbours array, hence only if neighbours_counter.GT.0
				if (neighbours_counter.GT.0) then
					do m=1, neighbours_counter
						if ((neighbours(m,1).EQ.parent_1).AND.(neighbours(m,2).EQ.parent_2)) then		!For all existing components of neighbours array, check that the neighbour pair you are about to add has not already been added
							GO TO 60																	!If they are the same then skip the procedure of adding this pair to the array.
						end if
					end do
				end if

				neighbours_counter=neighbours_counter+1		!Increment counter that records number of components in neighbours array.
				neighbours(neighbours_counter,1)=parent_1	!Set neighbours array components to labels of parent cells.
				neighbours(neighbours_counter,2)=parent_2
			else
				GO TO 60
			end if
		60	CONTINUE
		end do

		!At this point we have a nearest neighbour array with no redundancy, as required.

		!Create neighbour vector files for this snapshot
		write(vectors_filename,"(A45,I2.2)") "data/neighbour_vector_data/neighbour_vectors_", n_snapshots
		open(unit=39,file=trim(vectors_filename),status='unknown')

		!Now calculate proportion of near neighbours that are of the same type
		same_fate_counter=0
		do m=1, neighbours_counter

			fate_1 = cells(neighbours(m,1))%fate
			fate_2 = cells(neighbours(m,2))%fate
			cell_position1(:)	= cells(neighbours(m,1))%position(:)
			cell_position2(:)	= cells(neighbours(m,2))%position(:)

			do i=1, 3
				vector(i) = cell_position1(i) + 0.5*(cell_position2(i)-cell_position1(i))
			end do
			do i=4, 6
				vector(i) = cell_position2(i-3)-cell_position1(i-3)
			end do


			if (fate_1.EQ.fate_2) then
				same_fate_counter = same_fate_counter+1
			end if


			if (fate_1.eq.fate_2) then
				if (fate_1.eq.1) then
					pair_type = 1
				else
					pair_type = 2
				end if
			else
				pair_type = 3
			end if

			write(39,*) vector(:), pair_type

		end do

		sorting_measure = real(same_fate_counter)/real(neighbours_counter)
		!Write measurement to file sorting_data_neighbours
		write(38,*) real(time), sorting_measure
		close(39)
!		close(40)
!		close(41)
		call flush(39)
!		call flush(40)
!		call flush(41)
		call flush(38)
		deallocate(neighbours)

		end subroutine scem_measure_neighbours

end module scem_2_measure_neighbours
