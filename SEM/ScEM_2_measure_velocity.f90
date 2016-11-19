!Module to measure the average velocity of each cell type relative to the centre of mass of that cell type.
!CK Revell, October 2016

module scem_2_measure_velocity

  use scem_0_useful
  use scem_0_input
  use scem_1_types

  implicit none

contains

  subroutine scem_measure_velocity

    real*8, dimension(3) :: epi_com
    real*8, dimension(3) :: pre_com
    real*8, dimension(3) :: displacement
    real*8               :: radialvelocity,radius,parent_age
    integer              :: parent_fate,epi_count,pre_count

    !Calculate centres of mass for each cell type
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
    if (epi_count.GT.0) epi_com(:) = epi_com(:)/epi_count
    if (pre_count.GT.0) pre_com(:) = pre_com(:)/pre_count

    if (randomising) then
      open(unit=37,file=output_folder//'/randomised_data/sorting_data_velocity1.txt', status='unknown', position="append")
      open(unit=38,file=output_folder//'/randomised_data/sorting_data_velocity2.txt', status='unknown', position="append")
    else
      open(unit=37,file=output_folder//'/sorting_data/sorting_data_velocity1.txt', status='unknown', position="append")
      open(unit=38,file=output_folder//'/sorting_data/sorting_data_velocity2.txt', status='unknown', position="append")
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
