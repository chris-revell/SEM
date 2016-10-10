!Module to measure the distance of each cell type from that cell type's centre of mass
!CK Revell, October 2016

module scem_2_measure_type_radius

  use scem_0_useful
  use scem_0_input
  use scem_1_types

  implicit none

contains

  subroutine scem_measure_type_radius

    real*8, dimension(3) :: epi_com
    real*8, dimension(3) :: pre_com
    real*8, dimension(3) :: displacement
    integer              :: parent_fate,epi_count,pre_count

    open(unit=34, file=output_folder//"/system_data/sorting_data_type_radius1.txt", status="unknown", position="append")
    open(unit=35, file=output_folder//"/system_data/sorting_data_type_radius2.txt", status="unknown", position="append")

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
        displacement(:) = cells(n)%position(:) - epi_com(:)
        dist_sq = DOT_PRODUCT(displacement,displacement)
        write(34,*) time, SQRT(dist_sq)
      else
        displacement(:) = cells(n)%position(:) - epi_com(:)
        dist_sq = DOT_PRODUCT(displacement,displacement)
        write(35,*) time, SQRT(dist_sq)
      endif
    enddo

  end subroutine scem_measure_type_radius
end module scem_2_measure_type_radius
