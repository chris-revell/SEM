!Module to measure the distance between the centres of masses of the two cell types.
!CK Revell, November 2016

module scem_2_measure_com

  use scem_0_input
  use scem_0_arrays

  implicit none

contains

  subroutine scem_measure_com

    open(unit=39,file=output_folder//'/sorting_data/com.txt', status='unknown', position="append")

    write(39,*) time, SQRT(DOT_PRODUCT(pre_com-epi_com,pre_com-epi_com))

    close(unit=39)

  end subroutine scem_measure_com

end module scem_2_measure_com
