!Module to measure the proportion of the external system surface occupied by each cell type
!CK Revell, October 2016

module ScEM_2_measure_surface

  use scem_0_input
  use scem_1_types

  implicit none

contains

  subroutine scem_measure_surface

    integer              :: i,j
    integer              :: factor1,factor2,factor3
    real*8               :: epi_area,other_area
    real*8               :: pre_area
!    real*8               :: epi_out
!    real*8               :: pre_out
    real*8               :: area
    real*8, dimension(3) :: a
    real*8, dimension(3) :: b
    real*8, dimension(3) :: c

    if (.NOT.randomising) open(unit=43,file=output_folder//"/sorting_data/surface.txt",status="unknown",position="append")

    epi_area = 0
    pre_area = 0
    other_area=0

    do i=1, nc
      do j=1, cells(i)%triplet_count
        factor1 = elements(cells(i)%triplets(1,j))%DIT_factor
        factor2 = elements(cells(i)%triplets(2,j))%DIT_factor
        factor3 = elements(cells(i)%triplets(3,j))%DIT_factor

        if ((factor1.NE.0).AND.(factor2.NE.0).AND.(factor3.NE.0)) then
          !If all 3 elements in a triplet have DIT_factor.EQ.0, then this triplet is on the external system surface
          a = elements(cells(i)%triplets(1,j))%position - elements(cells(i)%triplets(2,j))%position
          b = elements(cells(i)%triplets(1,j))%position - elements(cells(i)%triplets(3,j))%position
          c = CROSS_PRODUCT(a,b)
          area = 0.5*SQRT(DOT_PRODUCT(c,c)) !0.5*|axb|
          !if (cells(i)%fate.EQ.1) then
            epi_area = epi_area + area
          !else
          !  pre_area = pre_area + area
          !endif
        else
          a = elements(cells(i)%triplets(1,j))%position - elements(cells(i)%triplets(2,j))%position
          b = elements(cells(i)%triplets(1,j))%position - elements(cells(i)%triplets(3,j))%position
          c = CROSS_PRODUCT(a,b)
          area = 0.5*SQRT(DOT_PRODUCT(c,c)) !0.5*|axb|
          other_area = other_area + area
        endif
      enddo
    enddo

  !  if (epi_area.GT.0.AND.pre_area.GT.0) then
  !    epi_out = real(epi_area)/real(epi_area+pre_area)
  !    pre_out = real(pre_area)/real(epi_area+pre_area)

  !    if (randomising) then
  !			surface_mean = surface_mean + pre_out
  !      surface_max  = MAX(surface_max,pre_out)
  !    else
        write(43,*) time, epi_area, other_area+epi_area
  !    endif

  !  endif

    close(43)

  end subroutine scem_measure_surface

end module ScEM_2_measure_surface
