!C.K. Revell, August 2013
!Version 2

module scem_4_cortex

	use scem_0_input
	use scem_0_arrays
	use scem_1_types
	use scem_3_polar

	implicit none

	contains

		subroutine scem_cortex

		integer, allocatable, dimension(:,:,:) :: bin_contents
		integer, allocatable, dimension(:,:) :: bin_counters
		real*8 R, R_max
		integer, allocatable, dimension(:,:) :: max_radius_elements
		integer	p, q

		do i=1, nc
			!Loop over all cells

			cells(i)%cortex_elements(0)=0		!Set cortex counter to zero before counting cortex elements later on in this module.
			do j=1, cells(i)%c_elements(0)
				elements(cells(i)%c_elements(j))%type = 1		!Refresh all elements to bulk cytoplasm type. If this is not done, there will be a steady gain of cortex elements over time because once an element is labelled as cortex it can never revert back to bulk.
			enddo

			call scem_polar(i)

			!Divide cell into solid angle bins.
			!Take the element with the largest radius in each bin to be a surface element.
			!This element and others within the cortex thickness set to type 2 - cortex.
			!The number of solid angle bins is chosen so that there are roughly 4 elements per bin on average.
			!So for 128 elements we want about 32 bins, which can be achieved with 4x8 slices.
			!Note that the label of an element in elements_polar is not the same as the true label of the element.

			!Bins are labelled by the number of divisions in the polar and azimuthal angles
			!So each bin has a label of the form (i,j) where in this case i runs from 1 to 4
			!covering polar angles from 0 to pi and j runs from 1 to 8 covering azimuthal angles
			!from 0 to less than and not equal to 2 pi.
			!Each bin also has a corresponding counter of the number of elements in that bin.
			!The counters are stored in bin_counters array.
			allocate(bin_contents(4, 8, cells(i)%c_elements(0)))
			allocate(bin_counters(4, 8))
			j=0
			k=0
			bin_counters(:,:) = 0

			do l=1, cells(i)%c_elements(0)				!Loop over all elements in the cell
				if (elements_polar(l,2).LE.(pi/4)) then
					j=1
					GO TO 20
				else
					if (elements_polar(l,2).LE.(pi/2)) then
						j=2
						GO TO 20
					else
						if (elements_polar(l,2).LE.(3*pi/4)) then
							j=3
							GO TO 20
						else
							if (elements_polar(l,2).LE.pi) then
								j=4
								GO TO 20
							end if
						end if
					end if
				end if

			20	CONTINUE
				if (elements_polar(l,3).LE.(pi/4)) then
					k=1
					GO TO 30
				else
					if (elements_polar(l,3).LE.(pi/2)) then
						k=2
						GO TO 30
					else
						if (elements_polar(l,3).LE.(3*pi/4)) then
							k=3
							GO TO 30
						else
							if (elements_polar(l,3).LE.(pi)) then
								k=4
								GO TO 30
							else
								if (elements_polar(l,3).LE.(5*pi/4)) then
									k=5
									GO TO 30
								else
									if (elements_polar(l,3).LE.(3*pi/2)) then
										k=6
										GO TO 30
									else
										if (elements_polar(l,3).LE.(7*pi/4)) then
											k=7
											GO TO 30
										else
											if (elements_polar(l,3).LT.(2*pi)) then	!Note last bin is has a "less than and not equal to" bound to prevent double counting
												k=8
												GO TO 30
											end if
										end if
									end if
								end if
							end if
						end if
					end if
				end if

			30	CONTINUE
				!At this point, (j,k) identifies the bin in which the element falls
				!Next step is to increase bin counter by 1 and allocate element to bin array
				!The array element in bin_contents to which the label of this SEM element is
				!added is given by the value of bin_counters(j,k) at this time
				bin_counters(j,k)=bin_counters(j,k)+1
				bin_contents(j,k,bin_counters(j,k))=l
			end do

			!At this stage we have an array containing a list of which elements lie in each bin
			!Next step is to determine which element in each bin has the greatest radius.

			R_max=0
			allocate(max_radius_elements(4,8))
			do j=1,4
				do k=1,8							!Do loop over all bins j,k
					R=0
					if (bin_counters(j,k).EQ.0) then
						GO TO 40
					else
						do l=1, bin_counters(j,k)		!Do loop over all elements in bin j,k
							if (elements_polar(bin_contents(j,k,l),1).GT.R) then
								R=elements_polar(bin_contents(j,k,l),1)
								max_radius_elements(j,k)=bin_contents(j,k,l)	!Update the array that contains the label of the element with the maximum radius
							end if
						end do
					end if
				40	Continue
					if (R.GT.R_max) then			!If the maximum radius in this bin exceeds previous max, reset max to be equal to the local max in this bin
						R_max=R
					end if
				end do
			end do

			!Should now have a value for the element that has the greatest
			!radius for the whole cell and also an array max_radius_elements
			!that gives the label of the element with the greatest radius in each bin

			!We now need to set elements to be cortex elements		************************
			!All elements in max_radius_elements are set to be surface elements		*********
			!unless their radius is less than half of R_max													*****************
			!(this is in order to exclude bins that by chance have no high radius elements in) *****
			!Furthermore, in order to include some thickness to the cortex, any element whose ******
			!radius is greater than 80% of the maximum radius of any element in its bin is *********
			!also set to be a surface element.																							*******

!			do j=1,4
!				do k=1,8							!Do loop over all bins j,k
!					if (bin_counters(j,k).EQ.0) then
!						GO TO 50
!					else
!						l=max_radius_elements(j,k)		!l is label of max radius element in this bin
!						if (elements_polar(l,1).GT.(0.5*R_max)) then	!Only set cortex elements in this bin if the max radius in the bin exceeds half of the max radius for the whole cell
!							do m=1, bin_counters(j,k)					!Loop over all elements in bin
!								n=bin_contents(j,k,m)					!m is the label of the element currently being considered - the mth element in bin (j,k)
!								if (elements_polar(n, 1).GT.(0.8*elements_polar(l,1))) then
!									elements(cells(i)%c_elements(n))%type=2					!Set all elements in this bin whose radius is greater than 80% of the max radius in the bin to be cortex elements. This naturally includes the outermost element and gives the cortex thickness
!									cells(i)%cortex_elements(0)=cells(i)%cortex_elements(0)+1		!Increment cortex counter by 1
!									p=cells(i)%cortex_elements(0)																!Current value of cortex counter (for conciseness in next couple of lines)
!									q=elements(cells(i)%c_elements(n))%label										!Label of element under consideration, whose type has just been set to 2 in line above
!									cells(i)%cortex_elements(p)=q																!pth element of cortex element array in cell data structure is set to the label of this cortex element q. So by the end cortex_elements contains a list of all cortex element labels.
!								end if
!							end do
!						end if
!					end if
!				50	Continue
!				end do
!			end do

			!This block of code allocates the highest radius element in each pyramid as cortex type without checking whether it is <50% of the max radius or checking if any others are >80% of the local max.
			do j=1,4
				do k=1,8							!Do loop over all bins j,k
					if (bin_counters(j,k).GT.0) then
							l=max_radius_elements(j,k)		!l is label of max radius element in this bin
							elements(l)%type=2					!Set all elements in this bin whose radius is greater than 80% of the max radius in the bin to be cortex elements. This naturally includes the outermost element and gives the cortex thickness
							cells(i)%cortex_elements(0)=cells(i)%cortex_elements(0)+1		!Increment cortex counter by 1
							p=cells(i)%cortex_elements(0)																!Current value of cortex counter (for conciseness in next couple of lines)
							q=elements(l)%label										!Label of element under consideration, whose type has just been set to 2 in line above
							cells(i)%cortex_elements(p)=q																!pth element of cortex element array in cell data structure is set to the label of this cortex element q. So by the end cortex_elements contains a list of all cortex element labels.
					endif
				enddo
			enddo

			deallocate(elements_polar)
			deallocate(bin_contents)
			deallocate(bin_counters)
			deallocate(max_radius_elements)
		end do

		!Should now have specified the type of all elements in all cells
		!Elements set to be of cortex type if their radius exceeds 80% of the max radius in that bin
		!and the max radius of that bin exceeds 50% of the max radius for the whole cell

		end subroutine scem_cortex

end module scem_4_cortex
