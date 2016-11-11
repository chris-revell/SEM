!CK Revell January 2015
!Module for applying the Promayon et al. (1996) method of volume conservation

module scem_4_volume_conserve

	use scem_0_arrays
	use scem_0_useful
	use scem_1_types

	implicit none

	contains

		subroutine scem_volume_conserve
			integer															::	c
			real*8, allocatable, dimension(:)		::	P_1				!Array of all dimensions of positions of all elements in cell after displacement by external forces, given in format (x1,y1,z1,x2,y2,z2...). Allocated with dimension 3n where n is the number of elements in the cell.
			real*8, allocatable, dimension(:)		::	P_2				!Array of all dimensions of positions of all elements in cell after volume conservation
			real*8, allocatable, dimension(:)		::	D					!Vector in 3n space that will move system from P_1 to P_2. D is proportional to the gradient of the surface of constant volume at point P_2.
			real*8															::	D_magnitude !Magnitude of vector D
			real*8															::	D_magnitude_sq	!Square of magnitude of vector D
			real*8, allocatable, dimension(:)		::	F 				!Vector in 3n space that describes movement of system due to external forces from original state to P_1
			real*8, allocatable, dimension(:)		::	E 				!Vector in 3n space that is the projection of vector F in the direction of D and will be used to return the system to a state approximately on the surface of constant volume.
			real*8															::	E_magnitude !Magnitude of vector E divided by the magnitude of vector D
			real*8, dimension(3)								::	P					!Vector position of element in triplet relative to centre of cell
			real*8, dimension(3)								::	Q					!Vector position of element in triplet relative to centre of cell
			real*8, dimension(3)								::	R					!Vector position of element in triplet relative to centre of cell
			integer															::	label_p		!Global label for element whose position is P
			integer															::	label_q		!Global label for element whose position is Q
			integer															::	label_r		!Global label for element whose position is P
			real*8															::	volume_fragment


			do c=1,nc

				allocate(P_1(3*cells(c)%cortex_elements(0)))
				allocate(P_2(3*cells(c)%cortex_elements(0)))
				allocate(D(3*cells(c)%cortex_elements(0)))
				allocate(E(3*cells(c)%cortex_elements(0)))
				allocate(F(3*cells(c)%cortex_elements(0)))
				D(:)=0

				do i=1, cells(c)%cortex_elements(0)
					P_1(3*(i-1)+1) = elements(cells(c)%cortex_elements(i))%position(1)		!x position of the ith cortex element, whose global label outside this module is cells(c)%cortex_elements(i)
					P_1(3*(i-1)+2) = elements(cells(c)%cortex_elements(i))%position(2)		!y position of the ith cortex element, whose global label outside this module is cells(c)%cortex_elements(i)
					P_1(3*(i-1)+3) = elements(cells(c)%cortex_elements(i))%position(3)		!z position of the ith cortex element, whose global label outside this module is cells(c)%cortex_elements(i)
				end do

				!The following do loop achieves the fundamental goal of calculating D, the derivative of the volume function.

				do i=1, cells(c)%cortex_elements(0)
					!Labels i of cortex elements in ltri() and cells()%triplets run from 1 to n where n is the number of cortex elements. They can be converted back to global labels with cells()%cortex_elements(i)
					!The following loop is used to test all triplets j from 1 to cells(c)%triplet_count
					do j=1, cells(c)%triplet_count
						!The following loop is used to test all 3 elements in triplet j to see if any of them is the same as element i.
						!If k=1 gives an element that is the same as element i there is no need to test the other two, so we use a GOTO command to quit the loop.
						do k=1, 3
							if(cells(c)%triplets(k,j).EQ.cells(c)%cortex_elements(i)) then
								!If element cells(c)%triplets(k,j) is element i then element i is part of this triangle
		            !and so this triplet will have a non-zero contribution to three
								!components in the derivative of the volume function corresponding to the 3 dimensions of element i.
								!This is because moving element i in any one of its 3 dimensions will change the value of the volume fragment
								!corresponding to this triplet.
								!Calculate vector positions of 3 triplet elements relative to the centre of the cell. One of these will be element i but it doesn't matter to us which.

								!The following if loops ensure that the position of element i is always labelled P, so we can always differentiate by P.
								if(k.EQ.1) then
									label_p = cells(c)%triplets(1,j)
									P(:)		= xe_prev(label_p,:)-cells(c)%position(:)									!!!!IS THIS THE CORRECT CELL POSITION TO USE
									label_q = cells(c)%triplets(2,j)
									Q(:)		= xe_prev(label_q,:)-cells(c)%position(:)
									label_r = cells(c)%triplets(3,j)
									R(:)		= xe_prev(label_r,:)-cells(c)%position(:)
								elseif(k.EQ.2) then
									label_p = cells(c)%triplets(2,j)
									P(:)		= xe_prev(label_p,:)-cells(c)%position(:)
									label_q = cells(c)%triplets(1,j)
									Q(:)		= xe_prev(label_q,:)-cells(c)%position(:)
									label_r	= cells(c)%triplets(3,j)
									R(:)		= xe_prev(label_r,:)-cells(c)%position(:)
								else !k.EQ.3
									label_p	= cells(c)%triplets(3,j)
									P(:)		= xe_prev(label_p,:)-cells(c)%position(:)
									label_q	= cells(c)%triplets(1,j)
									Q(:)		= xe_prev(label_q,:)-cells(c)%position(:)
									label_r	= cells(c)%triplets(2,j)
									R(:)		= xe_prev(label_r,:)-cells(c)%position(:)
								end if

								!The following calculation determines whether the dot product P.(QxR) gives a positive or negative result, and therefore whether this value requires a positive or negative sign when adding to the total volume sum (and hence volume derivative)
								volume_fragment=P(1)*Q(2)*R(3)-P(1)*Q(3)*R(2)+P(2)*Q(3)*R(1) &
																	-P(2)*Q(1)*R(3)+P(3)*Q(1)*R(2)-P(3)*Q(2)*R(1)


								if(volume_fragment.LT.0) then
									!Product is negative, so we add it to the volume sum with a negative sign to ensure positive volume contribution
									!Hence need a negative sign in front of the corresponding term in the derivative.
									!Derivative of volume fragment with respect to coordinates of P is +/-(QxR)
									D(3*i-2)=D(3*i-2)-(Q(2)*R(3)-Q(3)*R(2))
									D(3*i-1)=D(3*i-1)-(Q(3)*R(1)-Q(1)*R(3))
									D(3*i)	=D(3*i)	 -(Q(1)*R(2)-Q(2)*R(1))
								else
									!Product is positive, so need a positive sign when adding its contribution to the derivative.
									D(3*i-2)=D(3*i-2)+(Q(2)*R(3)-Q(3)*R(2))
									D(3*i-1)=D(3*i-1)+(Q(3)*R(1)-Q(1)*R(3))
									D(3*i)	=D(3*i)	 +(Q(1)*R(2)-Q(2)*R(1))
								end if

								EXIT
							else
								CYCLE
							endif
						end do
					end do
				enddo

				!Project the displacement vector of the system from external forces along D to find a vector that can be added to the system vector to appoximately move the system back onto the surface of constant volme.
				!First calculate F by taking the difference of P_1 and xe_prev (positions of elements before external forces)
				do l=1, cells(c)%cortex_elements(0)
				enddo
				do l=1, cells(c)%cortex_elements(0)
					F(l*3-2) = elements(cells(c)%cortex_elements(l))%position(1) - &
										xe_prev(cells(c)%cortex_elements(l),1)
					F(l*3-1) = elements(cells(c)%cortex_elements(l))%position(2) - &
										xe_prev(cells(c)%cortex_elements(l),2)
					F(l*3)	 = elements(cells(c)%cortex_elements(l))%position(3) - &
										xe_prev(cells(c)%cortex_elements(l),3)
				enddo

				!Now calculate E from F and D
				D_magnitude_sq=0
				do l=1, 3*cells(c)%cortex_elements(0)
					D_magnitude_sq = D_magnitude_sq + D(l)**2
				enddo
				D_magnitude=SQRT(D_magnitude_sq)
				E_magnitude = DOT_PRODUCT(F,D)/D_magnitude
				E(:)				=	E_magnitude*D(:)/D_magnitude

				!Use E to find P_2
				P_2(:) = P_1(:) + E(:)   !******IS THIS THE CORRECT SIGN************

				!Use P_2 to update global position data
				do l=1, cells(c)%cortex_elements(0)
					elements(cells(c)%cortex_elements(l))%position(1)=P_2(3*(l-1)+1)
					elements(cells(c)%cortex_elements(l))%position(2)=P_2(3*(l-1)+2)
					elements(cells(c)%cortex_elements(l))%position(3)=P_2(3*(l-1)+3)
				end do

				deallocate(D)
				deallocate(E)
				deallocate(F)

			enddo

		end subroutine scem_volume_conserve
end module scem_4_volume_conserve
