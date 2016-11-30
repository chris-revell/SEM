! T. J Newman, Tempe, July 2010

module scem_2_flag_relist

  use scem_0_arrays
  use scem_0_input
  use scem_1_types

  implicit none

  contains

  !Subroutine to determine whether or not any element has moved far enough to trigger reconstruction of sector tables and pair array

    subroutine scem_flag_relist

      integer :: n
      real*8  :: sep_sq
      real*8, dimension(3) :: dx,pos_1,pos_2

      do n=1,ne
         pos_1(:)=elements(n)%position(:)			!Note that these are the latest positions of the elements - xe_compare contains the old positions of the elements
         pos_2(:)=xe_compare(n,:)					!Remember from scem_relist(arg), xe_compare(n,:)=elements(n)%position(:) for all ne elements
         dx=pos_1(:)-pos_2(:)						!Vector displacement from old element positions to new element positions
         sep_sq=dot_product(dx,dx)					!The length of this displacement vector, squared. ie the scalar distance between the old element position and new element position, squared
         if (sep_sq.gt.0.25*buffer_size_sq) then
            flag_relist=1							!If any element has moved by more than a specified distance then flag_relist is set to 1, which triggers reconstructing of sector tables and pair array in scem_iterate
         end if
      end do

    end subroutine scem_flag_relist

end module scem_2_flag_relist
