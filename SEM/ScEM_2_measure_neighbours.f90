!C.K.Revell, March 2014

module scem_2_measure_neighbours

	use scem_0_input
	use scem_0_arrays
	use scem_1_types

	implicit none

contains

	subroutine scem_measure_neighbours

		integer 								:: parent1,parent2
		integer									:: fate1,fate2
		integer									:: i,j,n
		real*8,dimension(3)			:: dx
		integer, dimension(2,2) :: neighbour_counts


		!Start by looking at all element-element interaction pairs.
		!If a pair acts between elements in cells of different parents i and j where i=!j, set neighbours(i,j) = .TRUE.
		neighbours(:,:) = 0
		do n=1, np
			!Find labels of parent cells for elements in interaction pair
			parent1 = elements(pairs(n,1))%parent
			parent2 = elements(pairs(n,2))%parent
			dx(:) = elements(pairs(n,1))%position - elements(pairs(n,2))%position
			if (parent1.NE.parent2.AND.DOT_PRODUCT(dx,dx).LT.r_interaction_max_sq) then
				neighbours(parent1,parent2) = 1
			endif
		enddo
		!neighbours(i,j) = .TRUE. for any i,j where cell i and cell j are nearest neighbours.

		!Use neighbours array to sum epi-epi, pre-pre and epi-pre nearest neighbour cell pairs
		neighbour_counts(:,:) = 0
		do i=1,nc
			do j=i+1,nc
				if (neighbours(i,j).EQ.1) then
					fate1 = cells(i)%fate
					fate2 = cells(j)%fate
					neighbour_counts(fate1,fate2) = neighbour_counts(fate1,fate2) + 1
				endif
			enddo
		enddo
		!neighbour_counts(i,j) now equals the total number of nearest neighbour pairs containing a cell of fate i and a cell of fate j
		!Need to sum neighbour_counts(1,2) + neighbour_counts(2,1) to obtain unlike pair count.

		!Output measurements
		if (randomising) then
			random_values_neighbours(ran_loop,:) = (/neighbour_counts(1,1), neighbour_counts(2,2)/)
		else
			open(unit=36,file=output_folder//'/sorting_data/neighbours.txt',status='unknown',position="append")
			write(36,"(*(G0,:,1X))") time, neighbour_counts(1,1), neighbour_counts(2,2), neighbour_counts(2,1)+neighbour_counts(1,2)
			close(36)
		endif

	end subroutine scem_measure_neighbours

end module scem_2_measure_neighbours
