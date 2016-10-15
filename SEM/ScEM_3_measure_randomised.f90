!Module to take the system at each output interval, randomly reallocate the fates of all cells, reapply the various measurements, and then return the system to normal.
!This protocol establishes a baseline for how the measurements would look in a random system in which no sorting is occuring.
!CK Revell, October 2016

module scem_3_measure_randomised

  use scem_0_arrays
  use scem_0_input
  use scem_0_useful
  use scem_1_types
  use scem_2_measure_radius
  use scem_2_measure_neighbours
  use scem_2_measure_displacement
  use scem_2_measure_type_radius
  use scem_2_measure_surface

  implicit none

contains

  subroutine scem_measure_randomised



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
    do n=1,nc
      stored_fates(n) = cells(n)%fate
    enddo


    do i=1, 10 !This should probably be nc! but even for 12 cells that ends up being over 4000 possibilities
      do n=1,nc
        CALL RANDOM_NUMBER(fate_decider)
        if (fate_decider.GE.0.5) then
          cells(n)%fate = 1
        else
          cells(n)%fate = 2
        endif
      enddo

      !Perform sorting measurements on newly randomised system
      !Sorting measurements
      if (flag_measure_radius.EQ.1) then
        call scem_measure_radius
      endif
      if (flag_measure_neighbours.EQ.1) then
        call scem_measure_neighbours
      endif
      if (flag_measure_displacement.EQ.1) then
        call scem_measure_displacement
      endif
      if (flag_measure_type_radius.EQ.1) then
        call scem_measure_type_radius
      endif
      if (flag_measure_surface.EQ.1) then
        call scem_measure_surface
      endif

    enddo

    !Restore original system state.
    do n=1,nc
      cells(n)%fate = stored_fates(n)
    enddo

    randomising = .FALSE.

    deallocate(stored_fates)

  end subroutine scem_measure_randomised
end module scem_3_measure_randomised
