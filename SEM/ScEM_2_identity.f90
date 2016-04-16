! T. J Newman, Tempe, September 2010

! create identity array c_elements as part of 'cells' data structure

module scem_2_identity

  use scem_0_arrays
  use scem_0_input
  use scem_0_useful
  use scem_1_types

  implicit none

  contains

    subroutine scem_identity

      integer, allocatable, dimension(:) :: counter

      allocate(counter(nc))
      counter(:)=0

      ! transfer element's parent information to a register for each cell

      do n=1,ne
         k=elements(n)%parent
         counter(k)=counter(k)+1
         cells(k)%c_elements(counter(k))=n
      end do


      ! record total number of elements in each cell
      do k=1,nc
         cells(k)%c_elements(0)=counter(k)
      end do

      deallocate(counter)

    end subroutine scem_identity
    
end module scem_2_identity
