! T. J Newman, Dundee, February 2011

module scem_2_division

  use scem_0_arrays
  use scem_0_input
  use scem_1_types

  implicit none

  contains

    subroutine scem_division

      integer, allocatable, dimension(:) :: c_el_temp1(:),c_el_temp2(:)
      integer :: nc_old
      real*8, dimension(3) :: pos,long_axis,relative_pos,x_com
      real*8 :: max_sep,max_sep_old,epsilon
      integer :: i,j,k,m,n,nn
      !real*8  :: fate_decider

      allocate(c_el_temp1(0:4*ne_cell))
      allocate(c_el_temp2(0:4*ne_cell))

      nc_old=nc
      max_sep=0.d0
      max_sep_old=max_sep

      do k=1,nc_old
         if (cells(k)%c_elements(0).ge.2*ne_cell) then

            nc=nc+1

            if (intro) write(*,'(I2)') nc

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

            if (flag_symmetric_division.EQ.1) then
              cells(nc)%fate=cells(k)%fate
              cells(nc)%label=nc
            else
              CALL RANDOM_NUMBER(fate_decider) !Fill fate_decider variable with a random number between 0 and 1.
              if (fate_decider.GE.0.5) then
                cells(nc)%fate=cells(k)%fate	!Symmetric division
              else
                cells(nc)%fate=MOD(cells(k)%fate,2)+1  !Asymmetric division. New cell has the opposite fate of parent cell.
              endif
            endif

            !Calculate the centres of mass of the two new daughter cells and store in the original_position component of the cells data type
            x_com(:)=0.0
            do m=1,cells(k)%c_elements(0)
              n=cells(k)%c_elements(m)
              x_com(:)=x_com(:)+elements(n)%position(:)
            end do
            x_com(:)=x_com(:)/cells(k)%c_elements(0)
            cells(k)%original_position(:)=x_com(:)
            x_com(:)=0.0
            do m=1,cells(nc)%c_elements(0)
              n=cells(nc)%c_elements(m)
              x_com(:)=x_com(:)+elements(n)%position(:)
            end do
            x_com(:)=x_com(:)/cells(nc)%c_elements(0)
            cells(nc)%original_position(:)=x_com(:)

         end if
      end do

      if (nc_old.EQ.1.AND.nc.GT.nc_old) then
        area_normalisation_factor = area_normalisation_factor/area_normalisation_count
        print*, area_normalisation_factor
      endif

      deallocate(c_el_temp1)
      deallocate(c_el_temp2)

    end subroutine scem_division

end module scem_2_division
