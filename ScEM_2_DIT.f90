!C.K. Revell November 2015
!Module containint subroutine scem_dit to work out which interfaces of a cell are
!shared with cells of a different fate, and hence change the cortical tension
!at these interfaces according to the differential interfactial tension hypothesis

module scem_2_DIT

  use scem_0_arrays
  use scem_0_input
  use scem_1_types


  implicit none

contains

  subroutine scem_dit

    integer :: j
    integer :: cell_1
    integer :: cell_2
    integer :: fate_1
    integer :: fate_2


    !For each element interaction pair, test to see if the parent cells of the two elements
    !in the pair are of the same fate.
    !If the fates of the two cells are different, we can update the "DIT_factor"
    !for each element in the pair to a value greater than 1.
    !This factor is a component of the "elements" data structure and is used
    !as a factor in the calculation of forces on elements.

    elements(:)%DIT_factor = 1

    do j=1,np

      cell_1  = elements(pairs(j,1))%parent
      fate_1  = cells(cell_1)%fate
      cell_2  = elements(pairs(j,2))%parent
      fate_2  = cells(cell_2)%fate

      if (fate_1.NE.fate_2) then
        elements(pairs(j,1))%DIT_factor = fate_1    !Making DIT factor depend on fate. One cell type increases tension, the other does not. 
        elements(pairs(j,2))%DIT_factor = fate_2    !
      else
        elements(pairs(j,1))%DIT_factor = 1
        elements(pairs(j,2))%DIT_factor = 1
      endif

    enddo

  end subroutine
end module scem_2_DIT
