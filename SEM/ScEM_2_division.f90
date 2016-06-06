! T. J Newman, Dundee, February 2011

module scem_2_division

  use scem_0_arrays
  use scem_0_input
  use scem_0_ran_array
  use scem_0_useful
  use scem_1_types


  implicit none

  contains

    subroutine scem_division

      real*8, dimension(3) :: pos,long_axis,relative_pos,x_com
      integer, allocatable, dimension(:) :: c_el_temp1(:),c_el_temp2(:)
      real :: fate_decider
      real, external :: rng

      integer :: nc_old
      real*8 :: max_sep,max_sep_old,epsilon

      allocate(c_el_temp1(0:4*ne_cell))
      allocate(c_el_temp2(0:4*ne_cell))

      nc_old=nc
      max_sep=0.d0
      max_sep_old=max_sep

      do k=1,nc_old
         if (cells(k)%c_elements(0).ge.2*ne_cell) then
            nc=nc+1
            do i=2,cells(k)%c_elements(0)
               n=cells(k)%c_elements(i)
               do j=1,i-1
                  nn=cells(k)%c_elements(j)
                  pos(:)=elements(n)%position(:)-elements(nn)%position(:)
                  max_sep=max(max_sep,dot_product(pos,pos))
                  if (max_sep.gt.max_sep_old) then
                     long_axis(:)=pos(:)
                  end if
                  max_sep_old=max_sep
               end do
            end do
            x_com(:)=cells(k)%position(:)
            c_el_temp1(:)=0
            c_el_temp2(:)=0
            do i=1,cells(k)%c_elements(0)
               n=cells(k)%c_elements(i)
               relative_pos(:)=elements(n)%position(:)-x_com(:)
               epsilon=dot_product(relative_pos,long_axis)
               if (epsilon.ge.0.0) then
                  c_el_temp1(0)=c_el_temp1(0)+1
                  c_el_temp1(c_el_temp1(0))=n
                  elements(n)%parent=k
               else
                  c_el_temp2(0)=c_el_temp2(0)+1
                  c_el_temp2(c_el_temp2(0))=n
                  elements(n)%parent=nc
               end if
            end do
            cells(k)%age=0.0
            cells(nc)%age=0.0
            cells(k)%c_elements(:)=c_el_temp1(:)
            cells(nc)%c_elements(:)=c_el_temp2(:)

            !fate_decider is a random number between 0 and 1. If this random number is greater than 0.2
            !then division is symmetric, otherwise it is asymmetric.
            !This models the 80:20 split for symmetric vs asymmetric observed for ICM cell division
!            fate_decider = rng(iseed)
            CALL RANDOM_SEED()
            CALL RANDOM_NUMBER(fate_decider)
      			if (fate_decider.GT.0.5) then
      				cells(nc)%fate=cells(k)%fate	!Symmetric division
      			else if (cells(k)%fate.eq.1) then
      					cells(nc)%fate=2			!Asymmetric division where parent cell has type 1
      				 else
      				 	cells(nc)%fate=1			!Asymmetric division where parent cell has type 2
      			end if

            cells(nc)%label=nc

         end if
      end do

      deallocate(c_el_temp1)
      deallocate(c_el_temp2)

    end subroutine scem_division

end module scem_2_division
