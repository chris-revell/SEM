!Module to measure the displacement of a cell from its original position against its age
!CK Revell October 2016

module scem_2_measure_displacement

  use scem_0_input
  use scem_1_types
  use scem_2_com

  implicit none

contains

  subroutine scem_measure_displacement

    integer :: n, count1, count2
    real*8  :: displacement1, displacement2
    real*8, dimension(3) :: displacement_vector

    open(unit=41, file=output_folder//"/sorting_data/displacement.txt", status="unknown", position="append")
    displacement1 = 0
    displacement2 = 0
    count1=0
    count2=0
    do n=1, nc
      displacement_vector(:) = cells(n)%position(:) - cells(n)%original_position(:)
      if (cells(n)%fate.EQ.1) then
        displacement1 = displacement1 + DOT_PRODUCT(displacement_vector,displacement_vector)
        count1 = count1+1
      else
        displacement2 = displacement2 + DOT_PRODUCT(displacement_vector,displacement_vector)
        count2 = count2+1
      endif
    enddo
    write(41,*) cells(0)%age, displacement1/count1, displacement2/count2

    close(41)

  end subroutine scem_measure_displacement

end module scem_2_measure_displacement
