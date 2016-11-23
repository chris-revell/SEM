! T. J Newman, Tempe, July 2010

module scem_4_integrate

  use scem_2_background
  use scem_3_near_neighbour_update
  use scem_3_cortical_tension_update

  implicit none

  contains

    subroutine scem_integrate

      !Update element velocities according to standard near neighbour interactions.
      call scem_near_neighbour_update

      !Update cortex element velocities according to cortex interaction network.
      call scem_cortical_tension_update

      !Update element velocities according to background potential.
      call scem_background(flag_background)

    end subroutine scem_integrate

end module scem_4_integrate
