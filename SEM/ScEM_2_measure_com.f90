!Module to measure the distance between the centres of masses of the two cell types.
!CK Revell, November 2016

module scem_2_measure_com

  use scem_0_input
  use scem_0_arrays
  use scem_1_types

  implicit none

contains

  subroutine scem_measure_com

    real*8 :: intercelldistance_sq

    open(unit=39,file=output_folder//'/sorting_data/com.txt', status='unknown', position="append")

    intercelldistance_sq = DOT_PRODUCT(cells(1)%position-cells(2)%position,cells(1)%position-cells(2)%position)
    write(39,*) time, SQRT(intercelldistance_sq)

    close(unit=39)

  end subroutine scem_measure_com

end module scem_2_measure_com
