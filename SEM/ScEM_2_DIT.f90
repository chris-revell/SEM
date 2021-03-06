!C.K. Revell November 2015
!Module containing subroutine scem_dit to work out which interfaces of a cell are
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
    integer :: DIT_index1,DIT_index2

    !For each standard near-neighbour element interaction pair, test to see if the parent
    !cells of the two elements in the pair are of the same fate.
    !If the fates of the two cells are the same, we can update the "DIT_factor"
    !for each element in the pair to a value greater than 1.
    !This factor is a component of the "elements" data structure and is used
    !as a factor in the calculation of forces on elements.

    !Refresh DIT_factor values for all elements
    FORALL(j=1:ne) elements(j)%DIT_factor = 0

    do j=1,np
      cell_1  = elements(pairs(j,1))%parent
      fate_1  = cells(cell_1)%fate
      cell_2  = elements(pairs(j,2))%parent
      fate_2  = cells(cell_2)%fate
      if (cell_1.EQ.cell_2) then
        !Do nothing for pairs within the same cell
        CYCLE
      elseif (fate_1.EQ.fate_2) then
        !Set DIT_factor to be 1 at homotypic interfaces
        elements(pairs(j,1))%DIT_factor = 1
        elements(pairs(j,2))%DIT_factor = 1
      else
        !Set DIT_factor to be 2 at heterotypic interfaces
        elements(pairs(j,1))%DIT_factor = 2
        elements(pairs(j,2))%DIT_factor = 2
      endif
    enddo

    do j=1, np_cortex
      if (elements(pairs_cortex(j)%label1)%DIT_factor.EQ.elements(pairs_cortex(j)%label2)%DIT_factor) then
      !Interface defined where both elements have the same DIT_factor value
        DIT_index1 = cells(elements(pairs_cortex(j)%label1)%parent)%fate
        DIT_index2 = elements(pairs_cortex(j)%label1)%DIT_factor
        pairs_cortex(j)%cortex_factor = DIT_response(DIT_index1,DIT_index2)
      else
      !Cortex pairs in which both elements do not share the same DIT_factor are considered to lie between interfaces, so have the baseline cortical tension
        pairs_cortex(j)%cortex_factor = 1.0
      endif
    enddo

  end subroutine
end module scem_2_DIT
