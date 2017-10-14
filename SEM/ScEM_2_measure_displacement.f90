!Module to measure the displacement of a cell from its original position against its age
!CK Revell October 2016

module scem_2_measure_displacement

  use scem_0_input
  use scem_1_types

  implicit none

contains

  subroutine scem_measure_displacement

    integer :: n
    real*8  :: displacement
    real*8, dimension(3) :: displacement_vector

    open(unit=41, file=output_folder//"/sorting_data/displacement.txt", status="unknown", position="append")
    do n=1, nc
      displacement_vector(:) = cells(n)%position(:) - cells(n)%original_position(:)
      displacement = SQRT(DOT_PRODUCT(displacement_vector,displacement_vector))
      write(41,*) MIN(cells(n)%age,time), cells(n)%fate, displacement
    enddo
    close(41)

  end subroutine scem_measure_displacement

end module scem_2_measure_displacement
