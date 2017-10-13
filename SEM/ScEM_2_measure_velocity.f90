!Module to measure the average velocity of each cell type relative to the centre of mass of that cell type.
!CK Revell, October 2016

module scem_2_measure_velocity

  use scem_0_input
  use scem_0_arrays
  use scem_1_types

  implicit none

contains

  subroutine scem_measure_velocity

    integer                           :: parent,parent_fate,n
    real*8                            :: radialvelocity,radius,age
    real*8, dimension(3)              :: displacement
    real*8,allocatable,dimension(:,:) :: cell_velocities

!    if (randomising) then
!      open(unit=37,file=output_folder//'/randomised_data/velocity1.txt', status='unknown', position="append")
!      open(unit=38,file=output_folder//'/randomised_data/velocity2.txt', status='unknown', position="append")
!    else
      open(unit=37,file=output_folder//'/sorting_data/velocity1.txt', status='unknown', position="append")
      open(unit=38,file=output_folder//'/sorting_data/velocity2.txt', status='unknown', position="append")
!    endif

    if (allocated(cell_velocities)) then
				if (nc.GT.SIZE(cell_velocities,DIM=1)) then
					deallocate(cell_velocities)
					allocate(cell_velocities(nc,2))
				endif
		else
			!Array has not yet been allocated (ie, this is the start of the simulation)
			allocate(cell_velocities(nc,2))
		endif
    cell_velocities(:,:) = 0

    do n=1, ne
      parent      = elements(n)%parent
      parent_fate = cells(parent)%fate
      if (parent_fate.EQ.1) then
        displacement(:) = elements(n)%position(:) - epi_COM(:)
        radius = SQRT(DOT_PRODUCT(displacement,displacement))
        radialvelocity = DOT_PRODUCT(displacement(:)/radius,elements(n)%velocity(:)) !Unit vector from COM to element dotted with element velocity
        cell_velocities(parent,1) = cell_velocities(parent,1) + radialvelocity
        cell_velocities(parent,2) = cell_velocities(parent,2) + radius
      else
        displacement(:) = elements(n)%position(:) - pre_COM(:)
        radius = SQRT(DOT_PRODUCT(displacement,displacement))
        radialvelocity = DOT_PRODUCT(displacement(:)/radius,elements(n)%velocity(:)) !Unit vector from COM to element dotted with element velocity
        cell_velocities(parent,1) = cell_velocities(parent,1) + radialvelocity
        cell_velocities(parent,2) = cell_velocities(parent,2) + radius
      endif
    enddo

    do n=1,nc
      cell_velocities(n,1) = cell_velocities(n,1)/cells(n)%c_elements(0)
      cell_velocities(n,2) = cell_velocities(n,2)/cells(n)%c_elements(0)
      age = cells(n)%age
      if (cells(n)%fate.EQ.1) then
        write(37,*) cell_velocities(n,2), cell_velocities(n,1), MIN(age,time) !or just age without MIN()?
      else
        write(38,*) cell_velocities(n,2), cell_velocities(n,1), MIN(age,time) !or just age without MIN()?
      endif
    enddo

    close(37)
    close(38)

  end subroutine scem_measure_velocity
end module scem_2_measure_velocity
