! T. J Newman, Tempe, July 2010

! significantly revised, September 2010
! center of mass and radius of gyration incorporated into 'cells' data structure

module scem_2_com

  use scem_0_arrays
  use scem_0_input
  use scem_0_useful
  use scem_1_types

  implicit none

  contains

    subroutine scem_com

      real*8, dimension(3) :: x_com,dx
      real*8 :: rog

      do k=1,nc
         ! calculate center of mass
         x_com(:)=0.0
         do m=1,cells(k)%c_elements(0)
            n=cells(k)%c_elements(m)
            x_com(:)=x_com(:)+elements(n)%position(:)
         end do
         x_com(:)=x_com(:)/cells(k)%c_elements(0)
         cells(k)%position(:)=x_com(:)
         ! calculate radius of gyration
         rog=0.0
         do m=1,cells(k)%c_elements(0)
            n=cells(k)%c_elements(m)
            dx(:)=elements(n)%position(:)-x_com(:)
            rog=rog+dot_product(dx,dx)
         end do
         rog=sqrt(rog/cells(k)%c_elements(0))
         cells(k)%rad_gyration=rog
      end do

    end subroutine scem_com
    
end module scem_2_com
