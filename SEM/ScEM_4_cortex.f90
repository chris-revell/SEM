!C.K. Revell, August 2013
!Version 2

module scem_4_cortex

	use scem_0_input
	use scem_0_arrays
	use scem_1_types
	use scem_2_polar

	implicit none

	contains

		subroutine scem_cortex


		real*8 R, R_max
		integer	p, q
		integer :: element_label

		call scem_polar

		do i=1, nc
		!Loop over all cells
			cells(i)%cortex_elements(0)=0		!Set cortex counter to zero before counting cortex elements later on in this module.

			!Refresh all elements to bulk cytoplasm type. If this is not done, there will be a steady gain of cortex elements over time because once an element is labelled as cortex it can never revert back to bulk.
			do n=1, cells(i)%c_elements(0)
				elements(cells(i)%c_elements(n))%type = 1
			enddo

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
			j=0
			k=0
			bin_counters(:,:) = 0
			bin_contents(:,:,:) = 0

			do l=1, cells(i)%c_elements(0)				!Loop over all elements in the cell

				element_label = cells(i)%c_elements(l)

				if (elements(element_label)%polar(2).LE.(pi/4)) then
						j=1
				elseif (elements(element_label)%polar(2).LE.(pi/2)) then
						j=2
				elseif (elements(element_label)%polar(2).LE.(3*pi/4)) then
						j=3
				elseif (elements(element_label)%polar(2).LE.pi) then
						j=4
				end if

				if (elements(element_label)%polar(3).LE.(pi/4)) then
					k=1
				elseif (elements(element_label)%polar(3).LE.(pi/2)) then
					k=2
				elseif (elements(element_label)%polar(3).LE.(3*pi/4)) then
					k=3
				elseif (elements(element_label)%polar(3).LE.(pi)) then
					k=4
				elseif (elements(element_label)%polar(3).LE.(5*pi/4)) then
					k=5
				elseif (elements(element_label)%polar(3).LE.(3*pi/2)) then
					k=6
				elseif (elements(element_label)%polar(3).LE.(7*pi/4)) then
					k=7
				elseif (elements(element_label)%polar(3).LT.(2*pi)) then	!Note last bin is has a "less than and not equal to" bound to prevent double counting
					k=8
				end if

				!At this point, (j,k) identifies the bin in which the element falls
				!Next step is to increase bin counter by 1 and allocate element to bin array
				!The array element in bin_contents to which the label of this SEM element is
				!added is given by the value of bin_counters(j,k) at this time
				bin_counters(j,k)=bin_counters(j,k)+1
				bin_contents(j,k,bin_counters(j,k))=element_label

			end do !End loop over all elements in the cell.


			!At this stage we have an array containing a list of which elements lie in each bin
			!Next step is to determine which element in each bin has the greatest radius.
			R_max=0
			do j=1,4
				do k=1,8							!Do loop over all bins j,k
					R=0
					if (bin_counters(j,k).GT.0) then
						do l=1, bin_counters(j,k)		!Do loop over all elements in bin j,k
							if (elements(bin_contents(j,k,l))%polar(1).GT.R) then
								R=elements(bin_contents(j,k,l))%polar(1)
								max_radius_elements(j,k)=bin_contents(j,k,l)	!Update the array that contains the label of the element with the maximum radius
							end if
						end do
					end if
					if (R.GT.R_max) then			!If the maximum radius in this bin exceeds previous max, reset max to be equal to the local max in this bin
						R_max=R
					end if
				end do
			end do

			!Should now have a value for the element that has the greatest
			!radius for the whole cell and also an array max_radius_elements
			!that gives the label of the element with the greatest radius in each bin

			!We now need to set elements to be cortex elements
			!All elements in max_radius_elements are set to be surface elements
			!unless their radius is less than half of R_max
			!(this is in order to exclude bins that by chance have no high radius elements in)
			!Furthermore, in order to include some thickness to the cortex, any element whose
			!radius is greater than 80% of the maximum radius of any element in its bin is
			!also set to be a surface element.

			do j=1,4
				do k=1,8							!Do loop over all bins j,k
					if (bin_counters(j,k).GT.0) then !Check that there are elements in this bin
						l=max_radius_elements(j,k)		!l is label of max radius element in this bin
!						if (elements(element_label)%polar(1).GT.(0.5*R_max)) then	!Only set cortex elements in this bin if the max radius in the bin exceeds half of the max radius for the whole cell
							do m=1, bin_counters(j,k)					!Loop over all elements in bin
								n=bin_contents(j,k,m)					!m is the label of the element currently being considered - the mth element in bin (j,k)
								if (elements(n)%polar(1).GT.(0.8*elements(l)%polar(1))) then
									elements(cells(i)%c_elements(n))%type=2											!Set all elements in this bin whose radius is greater than 80% of the max radius in the bin to be cortex elements. This naturally includes the outermost element and gives the cortex thickness
									cells(i)%cortex_elements(0)=cells(i)%cortex_elements(0)+1		!Increment cortex counter by 1
									p=cells(i)%cortex_elements(0)																!Current value of cortex counter (for conciseness in next couple of lines)
									q=elements(cells(i)%c_elements(n))%label										!Label of element under consideration, whose type has just been set to 2 in line above
									cells(i)%cortex_elements(p)=q																!pth element of cortex element array in cell data structure is set to the label of this cortex element q. So by the end cortex_elements contains a list of all cortex element labels.
								end if
							end do
!						end if
					end if
				end do
			end do

		end do

		!Should now have specified the type of all elements in all cells
		!Elements set to be of cortex type if their radius exceeds 80% of the max radius in that bin
		!and the max radius of that bin exceeds 50% of the max radius for the whole cell

		end subroutine scem_cortex

end module scem_4_cortex
