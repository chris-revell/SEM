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

    if (randomising) then
      open(unit=41, file=output_folder//"/randomised_data/displacement1.txt", status="unknown", position="append")
      open(unit=42, file=output_folder//"/randomised_data/displacement2.txt", status="unknown", position="append")
    else
      open(unit=41, file=output_folder//"/sorting_data/displacement1.txt", status="unknown", position="append")
      open(unit=42, file=output_folder//"/sorting_data/displacement2.txt", status="unknown", position="append")
    endif

    do n=1, nc

      displacement_vector(:) = cells(n)%position(:) - cells(n)%original_position(:)
      displacement = (DOT_PRODUCT(displacement_vector,displacement_vector))

      if (cells(n)%fate.EQ.1) then
        write(41,*) MIN(cells(n)%age,time), displacement
      else
        write(42,*) MIN(cells(n)%age,time), displacement
      endif

    enddo

    close(41)
    close(42)

  end subroutine scem_measure_displacement

end module scem_2_measure_displacement
