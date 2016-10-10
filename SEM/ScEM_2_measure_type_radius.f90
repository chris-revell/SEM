!Module to measure the distance of each cell type from that cell type's centre of mass
!CK Revell, October 2016

module scem_2_measure_type_radius

  use

  implicit none

contains

  subroutine scem_measure_type_radius

    real*8, dimension(3) :: x_com
    real*8, dimension(3) :: epi_com
    real*8, dimension(3) :: pre_com
    real*8, dimension(3) :: displacement
    integer              :: parent_fate,epi_count,pre_count

    epi_com(:) = 0
    pre_com(:) = 0
    epi_count  = 0
    pre_count  = 0
    do n=1, ne
      parent_fate = cells(elements(n)%parent)%fate
      if (parent_fate.EQ.1) then
        epi_count = epi_count + 1
        epi_com(:) = epi_com(:) + elements(n)%position(:)
      else
        pre_count = pre_count + 1
        pre_com(:) = pre_com(:) + elements(n)%position(:)
      endif
    enddo
    epi_com(:) = epi_com(:)/pre_count
    pre_com(:) = pre_com(:)/pre_count

    do n=1, nc
      if (cells(n)%fate.EQ.1) then
