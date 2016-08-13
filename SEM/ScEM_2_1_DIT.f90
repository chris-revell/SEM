!C.K. Revell November 2015
!Module containint subroutine scem_dit to work out which interfaces of a cell are
!shared with cells of a different fate, and hence change the cortical tension
!at these interfaces according to the differential interfactial tension hypothesis

module scem_2_1_DIT

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
    character(len=3) :: arg1

    !For each standard near-neighbour element interaction pair, test to see if the parent
    !cells of the two elements in the pair are of the same fate.
    !If the fates of the two cells are the same, we can update the "DIT_factor"
    !for each element in the pair to a value greater than 1.
    !This factor is a component of the "elements" data structure and is used
    !as a factor in the calculation of forces on elements.

    !Refresh DIT_factor values for all elements
    do j=1, ne
      elements(j)%DIT_factor = .FALSE.
    enddo

    do j=1,np
      cell_1  = elements(pairs(j,1))%parent
      fate_1  = cells(cell_1)%fate
      cell_2  = elements(pairs(j,2))%parent
      fate_2  = cells(cell_2)%fate
      if (cell_1.EQ.cell_2) then
        !Do nothing for pairs within the same cell
        CYCLE
      elseif (fate_1.EQ.fate_2) then
        !Set DIT_factor to be .TRUE. for elements only if both elements are in different cells
        !and those cells have the same fate.
        elements(pairs(j,1))%DIT_factor = .TRUE.
        elements(pairs(j,2))%DIT_factor = .TRUE.
      else
        elements(pairs(j,1))%DIT_factor = .FALSE.
        elements(pairs(j,2))%DIT_factor = .FALSE.
      endif
    enddo

    do j=1, np_cortex
      if (elements(pairs_cortex(j)%label1)%DIT_factor.AND.elements(pairs_cortex(j)%label2)%DIT_factor) then
!      if (elements(pairs_cortex(j)%label1)%DIT_factor.OR.elements(pairs_cortex(j)%label2)%DIT_factor) then

        !Change cortical tension only for one cell type
!        if (cells(elements(pairs_cortex(j)%label1)%parent)%fate.EQ.1) then
!          call get_command_argument(1,arg1)
!          read(arg1,*) pairs_cortex(j)%cortex_factor
!        else
!          call get_command_argument(1,arg1)
!          read(arg1,*) pairs_cortex(j)%cortex_factor
!        endif

        call get_command_argument(1,arg1)
        read(arg1,*) pairs_cortex(j)%cortex_factor

      else
        pairs_cortex(j)%cortex_factor = 1.0
      endif
    enddo

  end subroutine
end module scem_2_1_DIT
