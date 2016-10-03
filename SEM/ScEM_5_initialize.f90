! T. J Newman, Tempe, July 2010

module scem_5_initialize

  use scem_0_arrays
  use scem_0_input
  use scem_0_useful
  use scem_1_initialconditions
  use scem_1_types
  use scem_2_com
  use scem_2_identity
  use scem_2_initial_create
  use scem_2_initial_exist
  use scem_2_output_system
  use scem_2_output_povray
  use scem_2_pairs
  use scem_2_relist
  use scem_4_cortex
  use volume_calculate_module

  implicit none

  contains

    subroutine scem_initialize

      ! initialize time
      time=0.0

      ! initialize numbers of cells, elements, and pairs
      if (flag_create.eq.1) then
         call scem_initial_create ! make new initial cell configuration
      else
         call scem_initial_exist ! read in data file of existing cell configuration
      end if

      ! allocate center of mass, x_prev, and x_compare arrays
      allocate(xe_prev(ne_size,3)) ! x_prev records prior values of positions
      allocate(xe_compare(ne_size,3)) ! x_compare records values of positions at time of sector assignments

      ! allocate sector and pairs arrays
      allocate(head(nx,ny,nz))
      allocate(list(ne_size))
      allocate(pairs(np_size,2))
      !pairs_cortex can only be allocated after volume_calculate has been called because it relies on the Delaunay triangulation.

      ! initialize position vector for center of sector grid
      x_cen(1)=sector_size*int(nx/2)
      x_cen(2)=sector_size*int(ny/2)
      x_cen(3)=sector_size*int(nz/2)

      ! create initial values for sector arrays
      call scem_relist(0)
      ! locate element pairs
      call scem_pairs

      ! identify elements with cell
      call scem_identity

      ! calculate initial center of mass and radius of gyration of cell(s)
      call scem_com

      !Find cortex elements
      call scem_cortex

	    ! Calculate initial cell volumes
      call volume_calculate

      ! write initial system data to file
      call scem_output_system

      ! Write element data to files in povray format
      if (flag_povray.EQ.1) then
        call scem_output_povray
      endif

      call scem_initialconditions

    end subroutine scem_initialize

end module scem_5_initialize
