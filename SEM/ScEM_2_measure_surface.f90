!Module to measure the proportion of the external system surface occupied by each cell type
!CK Revell, October 2016

module ScEM_2_measure_surface

  use scem_0_input
  use scem_0_useful
  use scem_1_types

  implicit none

contains

  subroutine scem_measure_surface

    integer :: factor1,factor2,factor3
    real*8 :: epi_area
    real*8 :: pre_area
    real*8 :: epi_out
    real*8 :: pre_out
    real*8 :: area
    real*8, dimension(3) :: a
    real*8, dimension(3) :: b
    real*8, dimension(3) :: c

    if (randomising) then
      open(unit=43,file=output_folder//"/randomised_data/sorting_data_surface.txt",status="unknown",position="append")
    else
      open(unit=43,file=output_folder//"/sorting_data/sorting_data_surface.txt",status="unknown",position="append")
    endif

    epi_area = 0
    pre_area = 0

    do i=1, nc
      do j=1, cells(i)%triplet_count
        factor1 = elements(cells(i)%triplets(1,j))%DIT_factor
        factor2 = elements(cells(i)%triplets(2,j))%DIT_factor
        factor3 = elements(cells(i)%triplets(3,j))%DIT_factor

        if ((factor1.EQ.0).OR.(factor2.EQ.0).OR.(factor3.EQ.0)) then
          !If all 3 elements in a triplet have DIT_factor.EQ.0, then this triplet is on the external system surface
          a = elements(cells(i)%triplets(1,j))%position - elements(cells(i)%triplets(2,j))%position
          b = elements(cells(i)%triplets(1,j))%position - elements(cells(i)%triplets(3,j))%position
          c = cross_product(a,b)
          area = 0.5*DOT_PRODUCT(c,c)
          if (cells(i)%fate.EQ.1) then
            epi_area = epi_area + area
          else
            pre_area = pre_area + 0.5*DOT_PRODUCT(c,c)
          endif
        else
          CYCLE
        endif
      enddo
    enddo

    if (epi_area.GT.0.AND.pre_area.GT.0) then
      epi_out = real(epi_area)/real(epi_area+pre_area)
      pre_out = real(pre_area)/real(epi_area+pre_area)
      write(43,*) time, epi_out, pre_out
    endif

    close(43)

  end subroutine scem_measure_surface

  function cross_product(vector1,vector2)
    real*8, dimension(3), intent(in) :: vector1
    real*8, dimension(3), intent(in) :: vector2
    real*8, dimension(3) :: cross_product

    cross_product(1) = vector1(2)*vector2(3)-vector1(3)*vector2(2)
    cross_product(2) = vector1(3)*vector2(1)-vector1(1)*vector2(3)
    cross_product(3) = vector1(1)*vector2(2)-vector1(2)*vector2(1)
  end function cross_product

end module ScEM_2_measure_surface
