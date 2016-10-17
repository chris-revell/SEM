!Module to measure the proportion of the external system surface occupied by each cell type
!CK Revell, October 2016

module ScEM_2_measure_surface

  use scem_0_input
  use scem_0_useful
  use scem_1_types

  implicit none

contains

  subroutine scem_measure_surface

    integer :: epi_count
    integer :: pre_count
    real*8  :: epi_out
    real*8  :: pre_out

    if (randomising) then
      open(unit=43,file=output_folder//"/randomised_data/sorting_data_surface.txt",status="unknown",position="append")
    else
      open(unit=43,file=output_folder//"/sorting_data/sorting_data_surface.txt",status="unknown",position="append")
    endif

    epi_count = 0
    pre_count = 0

    !Much of the following code mirrors a similar block in scem_dit
    do j=1, np_cortex
      if (elements(pairs_cortex(j)%label1)%DIT_factor.EQ.elements(pairs_cortex(j)%label2)%DIT_factor) then
        !Interface defined where both elements have the same DIT_factor value
        if (cells(elements(pairs_cortex(j)%label1)%parent)%fate.EQ.1) then
        !Surface of an epiblast cell
          if (elements(pairs_cortex(j)%label1)%DIT_factor.EQ.0) then
            !External surface of the system
            epi_count = epi_count + 1
          else
            CYCLE
          endif
        else
        !Surface of a PrE cell
          if (elements(pairs_cortex(j)%label1)%DIT_factor.EQ.0) then
            !External surface of the system
            pre_count = pre_count + 1
          else
            CYCLE
          endif
        endif
      else
        !Not at interface
        CYCLE
      endif
    enddo

    if (epi_count.GT.0.AND.pre_count.GT.0) then
      epi_out = real(epi_count)/real(epi_count+pre_count)
      pre_out = real(pre_count)/real(epi_count+pre_count)
      write(43,*) time, epi_out, pre_out
    endif

    close(43)

  end subroutine scem_measure_surface
end module ScEM_2_measure_surface
