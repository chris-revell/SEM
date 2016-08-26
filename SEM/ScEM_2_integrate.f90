! T. J Newman, Tempe, July 2010

module scem_2_integrate

  use scem_0_arrays
  use scem_0_input
  use scem_0_useful
  use scem_1_types
  use scem_2_background
  use scem_2_1_DIT
  use scem_2_1_near_neighbour_update
  use scem_2_2_cortical_tension_update

  implicit none

  contains

    subroutine scem_integrate

      !Update element velocities according to standard near neighbour interactions.
      call scem_near_neighbour_update

      !Update cortex element velocities according to cortex interaction network.
      if (flag_cortex.EQ.1) then
        call scem_cortical_tension_update
      endif

      !Update element velocities according to background potential.
      call scem_background(flag_background)

    end subroutine scem_integrate

end module scem_2_integrate
