! T. J Newman, Tempe, July 2010

module scem_2_deallocate

  use scem_0_arrays
  use scem_1_types

  implicit none

  contains

    subroutine scem_deallocate

      ! deallocate element and cell data arrays
      deallocate(elements)
      deallocate(cells)
      deallocate(potential_deriv)
      deallocate(xe_compare)
      deallocate(xe_prev)
      deallocate(head)
      deallocate(list)
      deallocate(pairs)
      deallocate(pairs_cortex)
      deallocate(rel_strength)

    end subroutine scem_deallocate

end module scem_2_deallocate
