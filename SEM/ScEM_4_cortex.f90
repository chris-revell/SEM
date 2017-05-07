!C.K. Revell, August 2013
!Version 2

module scem_4_cortex

	use scem_0_input
	use scem_0_arrays
	use scem_1_types
	use scem_2_polar
	use scem_3_delaunay

	implicit none

	contains

		subroutine scem_cortex

		integer	:: i,j,k,l,m,n

		call scem_polar

		do i=1, nc
		!Loop over all cells
			cells(i)%cortex_elements(:)=0		!Set cortex counter to zero before counting cortex elements later on in this module.

			!Refresh all elements to bulk cytoplasm type. If this is not done, there will be a steady gain of cortex elements over time because once an element is labelled as cortex it can never revert back to bulk.
			FORALL(n=1:cells(i)%c_elements(0)) elements(cells(i)%c_elements(n))%type = 1

			!Divide cell into solid angle bins.
			!Take the element with the largest radius in each bin to be a surface element.
			!This element and others within the cortex thickness set to type 2 - cortex.
			!The number of solid angle bins is chosen so that there are roughly 4 elements per bin on average.
			!So for 128 elements we want about 32 bins, which can be achieved with 4x8 slices.

			!Bins are labelled by the number of divisions in the polar and azimuthal angles
			!So each bin has a label of the form (i,j) where in this case i runs from 1 to 4
			!covering polar angles from 0 to pi and j runs from 1 to 8 covering azimuthal angles
			!from 0 to less than and not equal to 2 pi.
			!Each bin also has a corresponding counter of the number of elements in that bin.
			!The counters are stored in bin_counters array.
			bin_counters(:)   = 0
			bin_contents(:,:) = 0
			bin_max_radius(:) = 0

			!Allocate elements to a bin
			do l=1, cells(i)%c_elements(0)				!Loop over all elements in the cell

				n = cells(i)%c_elements(l)

				j = int(elements(n)%polar(2)/(pi/4))+1
				k = int(elements(n)%polar(3)/(pi/4))

				!Note that where previously the boundaries between bins were "less than or equal to" the upper boundary, they are now "less than"
				!This means that an element whose azimuthal or polar angle lies exactly on the boundary of a bin will now fall into the bin
				!above the one in which it would previously have been found, but this shouldn't be a problem since the chances of an angle
				!exactly coinciding with the boundary of a bin is vanishingly small.
				!We may also need to include terms for what to do if the polar angle is exactly equal to +/- pi or the azimuthal angle is equal to
				!precisely 2pi, since in both these cases j or k would take a value of 5 or 9 respectively, which would put it outside the bounds
				!of the bin_contents and bin_counters arrays. The chances of this should again be vanishingly small but it's worth bearing in mind
				!should the problem ever occur.

				!At this point, (j,k) identifies the bin in which the element falls
				!Next step is to increase bin counter by 1 and allocate element to bin array
				!The array element in bin_contents to which the label of this SEM element is
				!added is given by the value of bin_counters(j,k) at this time
				bin_counters(j+k)=bin_counters(j+k)+1
				bin_contents(j+k,bin_counters(j+k))=n
			end do !End loop over all elements in the cell.


			!At this stage we have an array containing a list of which elements lie in each bin
			!Next step is to determine which element in each bin has the greatest radius.

			do k=1,32							!Do loop over all bins
				do l=1, bin_counters(k)		!Do loop over all elements in bin
					bin_max_radius(k)=MAX(bin_max_radius(k),elements(bin_contents(k,l))%polar(1)) !Update the array that contains the max radius of elements within this bin
				end do
			end do

			!Should now have a value for the element that has the greatest
			!radius for the whole cell and also an array bin_max_radius
			!that gives the label of the element with the greatest radius in each bin

			!We now need to set elements to be cortex elements
			!All elements in bin_max_radius are set to be surface elements
			!unless their radius is less than half of R_max
			!(this is in order to exclude bins that by chance have no high radius elements in)
			!Furthermore, in order to include some thickness to the cortex, any element whose
			!radius is greater than 80% of the maximum radius of any element in its bin is
			!also set to be a surface element.

			do k=1,32							!Do loop over all bins
				do m=1, bin_counters(k)					!Loop over all elements in bin
					n=bin_contents(k,m)						!n is the label of the element currently being considered - the mth element in bin (j,k)
					if (elements(n)%polar(1).GT.(0.8*bin_max_radius(k))) then
						elements(n)%type=2																					!Set all elements in this bin whose radius is greater than 80% of the max radius in the bin to be cortex elements. This naturally includes the outermost element and gives the cortex thickness
						cells(i)%cortex_elements(0)=cells(i)%cortex_elements(0)+1		!Increment cortex counter by 1
						cells(i)%cortex_elements(cells(i)%cortex_elements(0))=n			!pth element of cortex element array in cell data structure is set to the label of this cortex element q. So by the end cortex_elements contains a list of all cortex element labels.
					end if
				end do
			end do

		end do

		!Should now have specified the type of all elements in all cells
		!Elements set to be of cortex type if their radius exceeds 80% of the max radius in that bin
		!and the max radius of that bin exceeds 50% of the max radius for the whole cell

		!Perform delaunay triangulation over the newly allocated set of cortex elements.
		call scem_delaunay

		end subroutine scem_cortex

end module scem_4_cortex
