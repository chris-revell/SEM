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

    integer :: n,i,epi_counter,epi_ran_counter
    real*8  :: fate_decider
    logical :: fatesnotbalanced

    !Set randomising = .TRUE. in order to divert output from measurement subroutines to randomised data files.
    randomising = .TRUE.

    !Store current system state
    !reallocate stored_fates array only if the number of cells in the system has increased.
    if (allocated(stored_fates)) then
				if (nc.GT.SIZE(stored_fates)) then
					deallocate(stored_fates)
					allocate(stored_fates(nc))
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


    do i=1, 1000 ! 10 seems to be an adequate number of tests, but could make it smaller to speed up the program

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
      enddo

      !Perform sorting measurements on newly randomised system. Randomised measurements not required for velocity or surface measurements.
      if (flag_measure_radius.EQ.1)       call scem_measure_radius

      if (flag_measure_neighbours.EQ.1)   call scem_measure_neighbours

!      if (flag_measure_displacement.EQ.1) call scem_measure_displacement

      if (flag_measure_type_radius.EQ.1)  call scem_measure_type_radius

      if (flag_measure_surface.EQ.1)      call scem_measure_surface

!      if (flag_measure_velocity.EQ.1)     call scem_measure_velocity

    enddo

    !Restore original system state.
    do n=1,nc
      cells(n)%fate = stored_fates(n)
    enddo

    randomising = .FALSE.

  end subroutine scem_measure_randomised
end module scem_3_measure_randomised
