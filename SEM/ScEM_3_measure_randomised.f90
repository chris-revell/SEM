!Module to take the system at each output interval, randomly reallocate the fates of all cells, reapply the various measurements, and then return the system to normal.
!This protocol establishes a baseline for how the measurements would look in a random system in which no sorting is occuring.
!CK Revell, October 2016

module scem_3_measure_randomised

  use scem_0_arrays
  use scem_0_input
  use scem_1_types
  use scem_2_measure_radius
  use scem_2_measure_neighbours
  use scem_2_measure_displacement
  use scem_2_measure_type_radius
  use scem_2_measure_surface
  use scem_2_measure_velocity
  use scem_2_measure_com

  implicit none

contains

  subroutine scem_measure_randomised

    integer                          :: n,i,epi_counter,epi_ran_counter,configuration,n_random
    real*8                           :: fate_decider
    logical                          :: fatesnotbalanced
    integer,allocatable,dimension(:) :: tested

    !Store current system state
    !reallocate stored_fates array only if the number of cells in the system has increased.
    if (ALLOCATED(stored_fates)) then
				if (nc.GT.SIZE(stored_fates)) then
					DEALLOCATE(stored_fates)
					ALLOCATE(stored_fates(nc))
				endif
		else
			!Array has not yet been allocated (ie, this is the start of the simulation)
			allocate(stored_fates(nc))
		endif
    epi_counter=0
    do n=1,nc
      stored_fates(n) = cells(n)%fate
      if (cells(n)%fate.EQ.1) epi_counter = epi_counter+1
    enddo

    !Set n_random. The number of random tests is set to be the minimum of nc choose n_epiblasts or 10000. This prevents an infinite loop when the number of possible configurations is smaller than 10000.
    !Use Stirling's approximation in binomial coefficient.
    n_random = MIN(20000,INT(0.95*(nc**(nc+0.5))/(SQRT(2*pi)*epi_counter**(epi_counter+0.5)*(nc-epi_counter)**(nc-epi_counter+0.5))))

    allocate(tested(n_random))

    !Set randomising = .TRUE. in order to divert output from measurement subroutines to randomised data files.
    randomising = .TRUE.
    tested(:) = 0

    open(unit=44, file=output_folder//'/randomised_data/radius.txt', status='unknown',position="append")
    open(unit=45, file=output_folder//'/randomised_data/neighbours.txt', status='unknown',position="append")
    open(unit=46, file=output_folder//'/randomised_data/surface.txt', status='unknown',position="append")
    radius1_mean = 0
    radius1_min = 1000
    radius2_mean = 0
    radius2_max = 0
    neighbours_mean = 0
    neighbours_max = 0
    surface_mean = 0
    surface_max = 0

    do i=1, n_random

      fatesnotbalanced = .TRUE.
      do while (fatesnotbalanced)
        epi_ran_counter = 0
        do n=1, nc
          CALL RANDOM_NUMBER(fate_decider)
          if (fate_decider.GE.0.5) then
            cells(n)%fate = 1
            epi_ran_counter = epi_ran_counter+1
          else
            cells(n)%fate = 2
          endif
        enddo
        if (epi_ran_counter.EQ.epi_counter) fatesnotbalanced = .FALSE.
        configuration = 0
        do n=1,nc
          configuration = configuration + (2**(n-1))*(cells(n)%fate-1)
        enddo
        do n=1,i
          if (configuration.EQ.tested(n)) then
            fatesnotbalanced = .TRUE.
            EXIT
          else
            CYCLE
          endif
        enddo
      enddo
      tested(i) = configuration

      !Perform sorting measurements on newly randomised system. Randomised measurements not required for velocity or surface measurements.
      if (flag_measure_radius.EQ.1)       call scem_measure_radius

      if (flag_measure_neighbours.EQ.1)   call scem_measure_neighbours

!      if (flag_measure_displacement.EQ.1) call scem_measure_displacement

!      if (flag_measure_type_radius.EQ.1)  call scem_measure_type_radius

      if (flag_measure_surface.EQ.1)      call scem_measure_surface

!      if (flag_measure_velocity.EQ.1)     call scem_measure_velocity

    enddo

    radius1_mean    = radius1_mean/n_random
    radius2_mean    = radius2_mean/n_random
    neighbours_mean = neighbours_mean/n_random
    surface_mean    = surface_mean/n_random
    write(44,"(*(G0,:,1X))") time,radius1_mean,radius1_min,radius2_mean,radius2_max
    write(45,"(*(G0,:,1X))") time,neighbours_mean,neighbours_max
    write(46,"(*(G0,:,1X))") time,surface_mean,surface_max
    close(44)
    close(45)
    close(46)


    !Restore original system state.
    do n=1,nc
      cells(n)%fate = stored_fates(n)
    enddo

    randomising = .FALSE.

  end subroutine scem_measure_randomised
end module scem_3_measure_randomised
