! T. J Newman, Tempe, July 2010

module scem_2_pairs

  use scem_0_arrays
  use scem_0_input
  use scem_0_useful
  use scem_1_types

  implicit none

  contains

    subroutine scem_pairs

      real*8, dimension(3) :: dx,pos_1,pos_2
      real*8 :: sep_sq
      integer, allocatable, dimension(:,:) :: pairs_temp
      integer, dimension(3) :: ixe
      integer :: np_size_trigger

      icount=0
      pairs(:,:)=0
      np_size_trigger=trigger_frac*np_size
      do n=1,ne-1
         pos_1(:)=elements(n)%position(:)
         ! adding 1 in next statement for more meaningful sector assignment
         ixe(:)=1+int((elements(n)%position(:)+x_cen(:))*recip_sector_size)
         do ix=-1,1
            do iy=-1,1
               do iz=-1,1
                  nn=head(ixe(1)+ix,ixe(2)+iy,ixe(3)+iz)
                  do while (nn.ne.0)
                     if (n.lt.nn) then
                        pos_2(:)=elements(nn)%position(:)
                        dx(:)=pos_1(:)-pos_2(:)
                        sep_sq=dot_product(dx,dx)
                        if (sep_sq.le.sector_size_sq) then
                           icount=icount+1
                           pairs(icount,1)=n
                           pairs(icount,2)=nn
                        end if
                     end if
                     nn=list(nn)
                  end do
               end do
            end do
         end do
         if (icount.gt.np_size_trigger) then
            allocate(pairs_temp(np_size,2))
            pairs_temp=pairs
            deallocate(pairs)
            allocate(pairs(2*np_size,2))
            pairs(1:np_size,:)=pairs_temp(1:np_size,:)
            pairs(np_size+1:2*np_size,:)=0
            deallocate(pairs_temp)
            np_size=2*np_size
            np_size_trigger=trigger_frac*np_size
         end if
      end do
      np=icount

    end subroutine scem_pairs

end module scem_2_pairs
