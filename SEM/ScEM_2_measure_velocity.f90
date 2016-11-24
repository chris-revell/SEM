!Module to measure the average velocity of each cell type relative to the centre of mass of that cell type.
!CK Revell, October 2016

module scem_2_measure_velocity

  use scem_0_useful
  use scem_0_input
  use scem_0_arrays
  use scem_1_types

  implicit none

contains

  subroutine scem_measure_velocity

    real*8, dimension(3) :: displacement
    real*8               :: radialvelocity,radius,parent_age
    integer              :: parent_fate

    if (randomising) then
      open(unit=37,file=output_folder//'/randomised_data/velocity1.txt', status='unknown', position="append")
      open(unit=38,file=output_folder//'/randomised_data/velocity2.txt', status='unknown', position="append")
    else
      open(unit=37,file=output_folder//'/sorting_data/velocity1.txt', status='unknown', position="append")
      open(unit=38,file=output_folder//'/sorting_data/velocity2.txt', status='unknown', position="append")
    endif

    do n=1, ne
      parent_fate = cells(elements(n)%parent)%fate
      parent_age  = cells(elements(n)%parent)%age
      if (parent_fate.EQ.1) then
        displacement(:) = elements(n)%position(:) - epi_COM(:)
        radius = SQRT(DOT_PRODUCT(displacement,displacement))
        radialvelocity = DOT_PRODUCT(displacement(:)/radius,elements(n)%velocity(:)) !Unit vector from COM to element dotted with element velocity
        write(37,*) radius, radialvelocity, MIN(parent_age,time)
      else
        displacement(:) = elements(n)%position(:) - pre_COM(:)
        radius = SQRT(DOT_PRODUCT(displacement,displacement))
        radialvelocity = DOT_PRODUCT(displacement(:)/radius,elements(n)%velocity(:)) !Unit vector from COM to element dotted with element velocity
        write(38,*) radius, radialvelocity, MIN(parent_age,time)
      endif
    enddo

    close(37)
    close(38)

  end subroutine scem_measure_velocity
end module scem_2_measure_velocity
