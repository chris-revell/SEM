!CK Revell January 2015
!Module for computing the volume of a cell in a particle based model from the boundary elements and centre of mass.

module scem_1_volume_calculate

	use scem_0_input
	use scem_0_arrays
	use scem_1_types

	implicit none

	contains

		subroutine scem_volume_calculate

			integer																:: i,j
			real*8																:: volume_cell			!Total volume of the cell
			real*8																:: volume_triplet		!Volume of tetrahedron formed by a given surface element triplet
			real*8, dimension(3)									:: P
			real*8, dimension(3)									:: Q					!3 vector positions of cortex elements in a neighbour triple
			real*8, dimension(3)									:: R
			real*8, dimension(3)									:: QcrossR				!Used in volume calculation. Cross product of Q and R.

			do j=1, nc
				volume_cell = 0
				do i=1, cells(j)%triplet_count

					volume_triplet=0

					!P,Q and R represent vectors from centre of cell to elements in surface triplet
					P = elements(cells(j)%triplets(1,i))%position - cells(j)%position
					Q = elements(cells(j)%triplets(2,i))%position - cells(j)%position
					R = elements(cells(j)%triplets(3,i))%position - cells(j)%position

					!Calculate cross product of Q and R
					QcrossR(1)=Q(2)*R(3)-Q(3)*R(2)
					QcrossR(2)=Q(3)*R(1)-Q(1)*R(3)
					QcrossR(3)=Q(1)*R(2)-Q(2)*R(1)

					!Volume of tetrahedron formed by surface triplet is (1/6)*P.QxR
					volume_triplet = (P(1)*QcrossR(1)+P(2)*QcrossR(2)+P(3)*QcrossR(3))/6.0

					volume_cell = volume_cell + ABS(volume_triplet)

				end do

				cells(j)%volume = volume_cell

			end do

		end subroutine scem_volume_calculate
end module scem_1_volume_calculate