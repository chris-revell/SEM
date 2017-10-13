!Module to take the system at each output interval, randomly reallocate the fates of all cells, reapply the various measurements, and then return the system to normal.
!This protocol establishes a baseline for how the measurements would look in a random system in which no sorting is occuring.
!CK Revell, October 2016

module scem_3_measure_randomised

  use scem_0_arrays
  use scem_0_input
  use scem_1_types
  use scem_2_measure_radius
  use scem_2_measure_neighbours
  use scem_2_measure_surface

  implicit none

contains

  subroutine scem_measure_randomised

    integer :: n,i,epi_ran_counter,configuration
    real*8  :: fate_decider
    logical :: fatesnotbalanced


    !Store current system state
    do n=1,nc
      stored_fates(n) = cells(n)%fate
    enddo

    neighbour_epi_below  = 0
    neighbour_pre_below  = 0
    surface_epi_below    = 0
    surface_pre_below    = 0
    radius_pre_below     = 0
    radius_pre_sys_below = 0
    radius_epi_below     = 0
    radius_epi_sys_below = 0

    !Set n_random. The number of random tests is set to be the minimum of nc choose n_epiblasts or 20000. This prevents an infinite loop when the number of possible configurations is smaller than 10000.
    !Use Stirling's approximation in binomial coefficient.
    if (n_random.LT.n_random_max.AND.nc.GT.20) n_random = n_random_max
    if (n_random.LT.n_random_max) n_random = MIN(20000,&
      INT(0.95*(nc**(nc+0.5))/(SQRT(2*pi)*epicellcount**(epicellcount+0.5)*(nc-epicellcount)**(nc-epicellcount+0.5))))

    !Set randomising = .TRUE. in order to divert output from measurement subroutines to randomised data files.
    randomising = .TRUE.
    tested(:) = 0

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
        if (epi_ran_counter.EQ.epicellcount) fatesnotbalanced = .FALSE.
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
      if (flag_measure_surface.EQ.1)      call scem_measure_surface
    enddo

    if (flag_measure_radius.EQ.1) then
      open(unit=44, file=output_folder//'/randomised_data/radius.txt',status='unknown',position="append")
      write(44,"(*(G0,:,1X))") time,radius_pre_below*100/n_random,radius_pre_sys_below*100/n_random,&
        radius_epi_below*100/n_random,radius_epi_sys_below*100/n_random
      close(44)
    endif
    if (flag_measure_neighbours.EQ.1) then
      open(unit=45, file=output_folder//'/randomised_data/neighbours.txt',status='unknown',position="append")
      write(45,"(*(G0,:,1X))") time,neighbour_epi_below*100.0/n_random,neighbour_pre_below*100.0/n_random
      close(45)
    endif
    if (flag_measure_surface.EQ.1) then
      open(unit=46, file=output_folder//'/randomised_data/surface.txt',status='unknown',position="append")
      write(46,"(*(G0,:,1X))") time,surface_epi_below*100.0/n_random,surface_pre_below*100.0/n_random
      close(46)
    endif

    !Restore original system state.
    do n=1,nc
      cells(n)%fate = stored_fates(n)
    enddo

    randomising = .FALSE.

  end subroutine scem_measure_randomised
end module scem_3_measure_randomised
