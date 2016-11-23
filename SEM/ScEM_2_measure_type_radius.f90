!Module to measure the distance of each cell type from that cell type's centre of mass
!CK Revell, October 2016

module scem_2_measure_type_radius

  use scem_0_useful
  use scem_0_input
  use scem_1_types
  use scem_2_com

  implicit none

contains

  subroutine scem_measure_type_radius

    real*8, dimension(3) :: displacement

    if (randomising) then
      open(unit=33, file=output_folder//"/randomised_data/type_radius1.txt", status="unknown", position="append")
      open(unit=34, file=output_folder//"/randomised_data/type_radius2.txt", status="unknown", position="append")
    else
      open(unit=33, file=output_folder//"/sorting_data/type_radius1.txt", status="unknown", position="append")
      open(unit=34, file=output_folder//"/sorting_data/type_radius2.txt", status="unknown", position="append")
    endif

    do n=1, nc
      if (cells(n)%fate.EQ.1) then
        displacement(:) = cells(n)%position(:) - epi_com(:)
        dist_sq = DOT_PRODUCT(displacement,displacement)
        write(33,*) time, SQRT(dist_sq), cells(n)%age
      else
        displacement(:) = cells(n)%position(:) - epi_com(:)
        dist_sq = DOT_PRODUCT(displacement,displacement)
        write(34,*) time, SQRT(dist_sq), cells(n)%age
      endif
    enddo

  end subroutine scem_measure_type_radius
end module scem_2_measure_type_radius
