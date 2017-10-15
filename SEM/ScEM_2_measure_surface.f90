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
    real*8               :: epi_area
    real*8               :: pre_area
    real*8               :: area
    real*8, dimension(3) :: a,b,c

    !if (.NOT.randomising) open(unit=43,file=output_folder//"/sorting_data/surface.txt",status="unknown",position="append")

    epi_area = 0
    pre_area = 0

    do i=1, nc
      do j=1, cells(i)%triplet_count
        factor1 = elements(cells(i)%triplets(1,j))%DIT_factor
        factor2 = elements(cells(i)%triplets(2,j))%DIT_factor
        factor3 = elements(cells(i)%triplets(3,j))%DIT_factor

        if ((factor1.EQ.0).AND.(factor2.EQ.0).AND.(factor3.EQ.0)) then
          !If all 3 elements in a triplet have DIT_factor.EQ.0, then this triplet is on the external system surface
          a = elements(cells(i)%triplets(1,j))%position - elements(cells(i)%triplets(2,j))%position
          b = elements(cells(i)%triplets(1,j))%position - elements(cells(i)%triplets(3,j))%position
          c = CROSS_PRODUCT(a,b)
          area = 0.5*SQRT(DOT_PRODUCT(c,c)) !0.5*|axb|
          if (cells(i)%fate.EQ.1) then
            epi_area = epi_area + area
          else
            pre_area = pre_area + area
          endif
        else
          CYCLE
        endif
      enddo
    enddo

    if (randomising) then
      if (pre_area.LT.surface_pre_measurement) surface_pre_below = surface_pre_below+1
      if (epi_area.GT.surface_epi_measurement) surface_epi_above = surface_epi_above+1
    else
      surface_pre_measurement = pre_area
      surface_epi_measurement = epi_area
    endif

    !close(43)

  end subroutine scem_measure_surface

end module ScEM_2_measure_surface
