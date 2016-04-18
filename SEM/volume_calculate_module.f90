!CK Revell January 2015
!Module for computing the volume of a cell in a particle based model from the boundary elements and centre of mass.

module volume_calculate_module

	use scem_0_input
	use scem_0_arrays
	use scem_1_types
	use trmesh_module
	use trlist2_module

	implicit none

	contains

		subroutine volume_calculate

!			integer, allocatable, dimension(:)		:: indices				!Array containing the global label for each cortex element. So for example the 2nd element in array x will be the 2nd cortex element but might have global label 5, so indices(2)=5
			integer																:: k					!Dummy variable for element label
			real*8																:: volume_cell			!Total volume of the cell
			real*8																:: volume_triplet		!Volume of tetrahedron formed by a given surface element triplet
			real*8, dimension(3)									:: P
			real*8, dimension(3)									:: Q					!3 vector positions of cortex elements in a neighbour triple
			real*8, dimension(3)									:: R
			real*8, dimension(3)									:: QcrossR				!Used in volume calculation. Cross product of Q and R.
			integer																:: i					!Dummy variable for do loop
			integer																:: j					!Dummy variable for do loop
			real*8, allocatable, dimension(:)			:: x_raw				!Raw x-dimension position data for cortex elements
			real*8, allocatable, dimension(:)			:: y_raw				!Raw y-dimension position data for cortex elements
			real*8, allocatable, dimension(:)			:: z_raw				!Raw z-dimension position data for cortex elements
			real*8, allocatable, dimension(:)			:: x					!Position data for cortex elements relative to COM and normalised to unit sphere
			real*8, allocatable, dimension(:)			:: y					!Position data for cortex elements relative to COM and normalised to unit sphere
			real*8, allocatable, dimension(:)			:: z					!Position data for cortex elements relative to COM and normalised to unit sphere
			real*8																:: radius_squared		!Square of distance from cell COM to a given element
			real*8																:: radius				!Distance from cell COM to a given element (for normalising to unit sphere)
			integer, allocatable, dimension(:)		:: list
			integer, allocatable, dimension(:)		:: lptr
			integer, allocatable, dimension(:)		:: lend
			integer																:: lnew
			integer, allocatable, dimension(:)		:: near
			integer, allocatable, dimension(:)		:: next
			real*8, allocatable, dimension(:)			:: dist
			integer																:: ier
			integer																:: nt					!Number of neighbour triplets in triangulation
			integer, allocatable, dimension(:,:)	:: ltri					!Array of delaunay triplets. 2nd index is label of triplet, first index is 1-3. So ltri(1,n), ltri(2,n), ltri(3,n) are labels of elements in neighbour triplet n, with ltri(1,n) being the smallest element label.
!		  character(len=1024)										:: delaunay_filename

			do j=1, nc

				!Allocate position arrays
				allocate(x_raw(cells(j)%cortex_elements(0)))
				allocate(y_raw(cells(j)%cortex_elements(0)))
				allocate(z_raw(cells(j)%cortex_elements(0)))
				allocate(x(cells(j)%cortex_elements(0)))
				allocate(y(cells(j)%cortex_elements(0)))
				allocate(z(cells(j)%cortex_elements(0)))


				!Extract x,y,z positions of cortex elements for arrays x_raw,y_raw,z_raw
				!Adjust position data to be position relative to cell centre of mass (position)
				do i=1, cells(j)%cortex_elements(0)
					k=cells(j)%cortex_elements(i)											!k is now the global label of the element under consideration
					x_raw(i) = elements(k)%position(1)-cells(j)%position(1)	!Evaluate x_raw,y_raw,z_raw and subtract cell centre of mass position
					y_raw(i) = elements(k)%position(2)-cells(j)%position(2)
					z_raw(i) = elements(k)%position(3)-cells(j)%position(3)
				end do

				!Normalise position data to unit sphere (required for trmesh)
				do i=1, cells(j)%cortex_elements(0)
					radius_squared	= x_raw(i)*x_raw(i)+y_raw(i)*y_raw(i)+z_raw(i)*z_raw(i)
					radius					= sqrt(radius_squared)
					x(i)						= x_raw(i)/radius
					y(i)						= y_raw(i)/radius
					z(i)						= z_raw(i)/radius
				end do

				!Allocate arrays for trmesh using system values
				allocate(list(6*(cells(j)%cortex_elements(0)-2)))
				allocate(lptr(6*(cells(j)%cortex_elements(0)-2)))
				allocate(lend(6*(cells(j)%cortex_elements(0)-2)))
				allocate(near(cells(j)%cortex_elements(0)))
				allocate(next(cells(j)%cortex_elements(0)))
				allocate(dist(cells(j)%cortex_elements(0)))
				allocate(ltri(3,(2*cells(j)%cortex_elements(0)-4)))

				!Call trmesh to perform triangulation
				call trmesh ( cells(j)%cortex_elements(0), x, y, z, list, lptr, lend, lnew, near, next, dist, ier )

				!Call trlist2 to extract usable triplet list ltri from trmesh results
				call trlist2 ( cells(j)%cortex_elements(0), list, lptr, lend, nt, ltri, ier )

				!Now that we have the list of Delaunay triangles, we can also allocate and fill the triplets array in the
				!cell data structure so that the triplets can be used in other modules.
				!Use cells(j)%cortex_elements() array to convert the label of elements in ltri(:,:) into the global element
				!label in cells(j)%triplets(:,:). This will make life much easier when the triplets array is used later on. 
				allocate(cells(j)%triplets(3,(2*cells(j)%cortex_elements(0)-4)))
				do i=1, nt
					cells(j)%triplets(1,i)=cells(j)%cortex_elements(ltri(1,i))
					cells(j)%triplets(2,i)=cells(j)%cortex_elements(ltri(2,i))
					cells(j)%triplets(3,i)=cells(j)%cortex_elements(ltri(3,i))
				end do
				cells(j)%triplet_count = nt

				!Having performed triangulation, now calculate cell volume
				volume_cell=0

				do i=1, nt

					volume_triplet=0

					P(1) = x_raw(ltri(1,i))		!P,Q and R represent vectors from centre of cell to elements in surface triplet
					P(2) = y_raw(ltri(1,i))
					P(3) = z_raw(ltri(1,i))
					Q(1) = x_raw(ltri(2,i))
					Q(2) = y_raw(ltri(2,i))
					Q(3) = z_raw(ltri(2,i))
					R(1) = x_raw(ltri(3,i))
					R(2) = y_raw(ltri(3,i))
					R(3) = z_raw(ltri(3,i))

					!Calculate cross product of Q and R
					QcrossR(1)=Q(2)*R(3)-Q(3)*R(2)
					QcrossR(2)=Q(3)*R(1)-Q(1)*R(3)
					QcrossR(3)=Q(1)*R(2)-Q(2)*R(1)

					volume_triplet = (P(1)*QcrossR(1)+P(2)*QcrossR(2)+P(3)*QcrossR(3))/real(6)	!Volume of tetrahedron formed by surface triplet is (1/6)*P.QxR

					volume_cell = volume_cell + ABS(volume_triplet)

				end do

				cells(j)%volume = volume_cell

				deallocate(x_raw)
				deallocate(y_raw)
				deallocate(z_raw)
				deallocate(x)
				deallocate(y)
				deallocate(z)
				deallocate(list)
				deallocate(lptr)
				deallocate(lend)
				deallocate(near)
				deallocate(next)
				deallocate(dist)
				deallocate(ltri)

			end do

		end subroutine volume_calculate
end module volume_calculate_module
