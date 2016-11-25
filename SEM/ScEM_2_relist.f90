! T. J Newman, Tempe, July 2010

module scem_2_relist

  use scem_0_arrays
  use scem_0_input
  use scem_1_types

  implicit none

  contains

    ! re-initialize ihead and recreate linked lists
    subroutine scem_relist(il)

      integer, intent(in)   :: il
      integer               :: n
      integer, dimension(3) :: ixe
      
      if (il.eq.1) then
         do n=1,ne
            if (list(n).eq.0) then
               ! adding 1 in next statement for more meaningful sector assignment
               ixe(:)=1+int((xe_compare(n,:)+x_cen(:))*recip_sector_size)
               head(ixe(1),ixe(2),ixe(3))=0
            end if
         end do
      else
         head=0
         do n=1,ne
         	ixe(:)=1+int((elements(n)%position(:)+x_cen(:))*recip_sector_size)
         end do
      end if

      do n=1,ne
         ! adding 1 in next statement for more meaningful sector assignment - int() rounds down.
         ixe(:)=1+int((elements(n)%position(:)+x_cen(:))*recip_sector_size)
         list(n)=head(ixe(1),ixe(2),ixe(3))
         head(ixe(1),ixe(2),ixe(3))=n
      end do

      flag_relist=0
      do n=1,ne
         xe_compare(n,:)=elements(n)%position(:)
      end do

    end subroutine scem_relist

end module scem_2_relist
