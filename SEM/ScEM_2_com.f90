! T. J Newman, Tempe, July 2010

! significantly revised, September 2010
! center of mass and radius of gyration incorporated into 'cells' data structure

! Calculates centres of mass for each cell type.

module scem_2_com

  use scem_0_arrays
  use scem_0_input

  use scem_1_types

  implicit none

  contains

    subroutine scem_com

      real*8, dimension(3) :: x_com,dx
      real*8               :: rog
      integer              :: parent_fate,epi_count,pre_count
      integer              :: k,m,n

      do k=1,nc
        ! calculate center of mass
        x_com(:)=0.0
        do m=1,cells(k)%c_elements(0)
          n=cells(k)%c_elements(m)
          x_com(:)=x_com(:)+elements(n)%position(:)
        end do
        x_com(:)=x_com(:)/cells(k)%c_elements(0)
        cells(k)%position(:)=x_com(:)

        if (time.EQ.0) then
          cells(k)%original_position(:) = x_com(:)
        endif

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


      ! If needed for measurements, calculate centres of mass for each cell type
      if (flag_measure_velocity.EQ.1.OR.flag_measure_type_radius.EQ.1) then
        epi_com(:) = 0
        pre_com(:) = 0
        epi_count  = 0
        pre_count  = 0
        do n=1, ne
          parent_fate = cells(elements(n)%parent)%fate
          if (parent_fate.EQ.1) then
            epi_count = epi_count + 1
            epi_com(:) = epi_com(:) + elements(n)%position(:)
          else
            pre_count = pre_count + 1
            pre_com(:) = pre_com(:) + elements(n)%position(:)
          endif
        enddo
        if (epi_count.GT.0) epi_com(:) = epi_com(:)/epi_count
        if (pre_count.GT.0) pre_com(:) = pre_com(:)/pre_count
      endif

    end subroutine scem_com

end module scem_2_com
