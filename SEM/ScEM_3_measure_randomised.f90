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
  use scem_2_output_povray

  implicit none

contains

  subroutine scem_measure_randomised

    integer :: n,epi_ran_counter,configuration
    real*8  :: fate_decider
    logical :: fatesnotbalanced
    real*8,dimension(4) :: means
    real*8,dimension(4) :: squaremeans
    real*8,dimension(4) :: stds
    real*8,dimension(4) :: maximums
    real*8,dimension(4) :: minimums

    !Store current system state
    do n=1,nc
      stored_fates(n) = cells(n)%fate
!      stored_fates_max_surf(n) = cells(n)%fate
!      stored_fates_max_rad(n) = cells(n)%fate
!      stored_fates_max_neighbour(n) = cells(n)%fate
    enddo

    random_values_surface    = 0
    random_values_radius     = 0
    random_values_neighbours = 0

!    neighbour_epi_below  = 0
!    neighbour_pre_above  = 0
!    surface_epi_above    = 0
!    surface_pre_below    = 0
!    radius_pre_below     = 0
!    radius_pre_sys_below = 0
!    radius_epi_above     = 0
!    radius_epi_sys_above = 0

!    rad_max = 0
!    surface_max = 0
!    epineighbourmax = 0

    !Set n_random. The number of random tests is set to be the minimum of nc choose n_epiblasts or 20000. This prevents an infinite loop when the number of possible configurations is smaller than 10000.
    !Use Stirling's approximation in binomial coefficient.
!    if (n_random.LT.n_random_max.AND.nc.GT.20) n_random = n_random_max
    if (n_random.LT.n_random_max) n_random = MIN(n_random_max,&
      INT(0.95*(nc**(nc+0.5))/(SQRT(2*pi)*epicellcount**(epicellcount+0.5)*(nc-epicellcount)**(nc-epicellcount+0.5))))

    !Set randomising = .TRUE. in order to divert output from measurement subroutines to randomised data files.
    randomising = .TRUE.
    tested(:) = 0

    do ran_loop=1, n_random
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
        do n=1,ran_loop
          if (configuration.EQ.tested(n)) then
            fatesnotbalanced = .TRUE.
            EXIT
          else
            CYCLE
          endif
        enddo
      enddo
      tested(ran_loop) = configuration

      !Perform sorting measurements on newly randomised system. Randomised measurements not required for velocity or surface measurements.
      if (flag_measure_radius.EQ.1)       call scem_measure_radius
      if (flag_measure_neighbours.EQ.1)   call scem_measure_neighbours
      if (flag_measure_surface.EQ.1)      call scem_measure_surface
    enddo

!    print*, n_random
!    if (flag_measure_radius.EQ.1) then
!      open(unit=44, file=output_folder//'/randomised_data/radius.txt',status='unknown',position="append")
!      write(44,"(*(G0,:,1X))") time,radius_epi_above*100.0/n_random,radius_epi_sys_above*100.0/n_random,&
!        radius_pre_below*100.0/n_random,radius_pre_sys_below*100.0/n_random
!      close(44)
!    endif
!    if (flag_measure_neighbours.EQ.1) then
!      open(unit=45, file=output_folder//'/randomised_data/neighbours.txt',status='unknown',position="append")
!      write(45,"(*(G0,:,1X))") time,neighbour_epi_below*100.0/n_random,neighbour_pre_above*100.0/n_random
!      close(45)
!    endif
!    if (flag_measure_surface.EQ.1) then
!      open(unit=46, file=output_folder//'/randomised_data/surface.txt',status='unknown',position="append")
!      write(46,"(*(G0,:,1X))") time,surface_epi_above*100.0/n_random,surface_pre_below*100.0/n_random
!      close(46)
!    endif
!
!    !output povray data for max value systems
!    do n=1,nc
!      cells(n)%fate = stored_fates_max_neighbour(n)
!    enddo
!    call scem_output_povray
!    n_snapshots=n_snapshots+1
!    do n=1,nc
!      cells(n)%fate = stored_fates_max_surf(n)
!    enddo
!    call scem_output_povray
!    n_snapshots=n_snapshots+1
!    do n=1,nc
!      cells(n)%fate = stored_fates_max_rad(n)
!    enddo
!    call scem_output_povray
!    n_snapshots=n_snapshots+1
!    !Restore original system state.
!    do n=1,nc
!      cells(n)%fate = stored_fates(n)
!    enddo

	  ! radius mean and standard deviations
    do n=1,4
      means(n) = SUM(random_values_radius(:,n))/n_random
      squaremeans(n) = SUM(random_values_radius(:,n)**2)/n_random
      maximums(n) = MAX(random_values_radius(:,n))
      minimums(n) = MIN(random_values_radius(:,n))
    enddo
    do n=1,4
      stds(n) = SQRT(ABS(squaremeans(n)-means(n)**2))
    enddo
    open(unit=44, file=output_folder//'/randomised_data/radius.txt',status='unknown',position="append")
    WRITE(44,"(*(G0,:,1X))") time,means(1),minimums(1),maximums(1),stds(1),means(2),minimums(2),maximums(2),stds(2),&
      means(3),minimums(3),maximums(3),stds(3),means(4),minimums(4),maximums(4),stds(4)
    close(44)

    do n=1,2
      means(n) = SUM(random_values_neighbours(:,n))/n_random
      squaremeans(n) = SUM(random_values_neighbours(:,n)**2)/n_random
      maximums(n) = MAX(random_values_neighbours(:,n))
      minimums(n) = MIN(random_values_neighbours(:,n))
    enddo
    do n=1,2
      stds(n) = SQRT(ABS(squaremeans(n)-means(n)**2))
    enddo
    open(unit=45, file=output_folder//'/randomised_data/neighbours.txt',status='unknown',position="append")
    WRITE(45,"(*(G0,:,1X))") time,means(1),minimums(1),maximums(1),stds(1),means(2),minimums(2),maximums(2),stds(2)
    close(45)

    means(1) = SUM(random_values_surface)/n_random
    squaremeans(1) = SUM(random_values_surface**2)/n_random
    stds(1) = SQRT(ABS(squaremeans(1)-means(1)**2))
    maximums(1) = MAX(random_values_surface(:,1))
    minimums(1) = MIN(random_values_surface(:,1))
    open(unit=46, file=output_folder//'/randomised_data/surface.txt',status='unknown',position="append")
    WRITE(46,"(*(G0,:,1X))") time,means(1),minimums(1),maximums(1),stds(1)
    close(46)

    randomising = .FALSE.

  end subroutine scem_measure_randomised
end module scem_3_measure_randomised
