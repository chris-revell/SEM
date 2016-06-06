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
      	 WRITE(*,*) "CREATE"
         nc=1
         ne=nc*ne_cell
         np=ne
      else
      	 WRITE(*,*) "READ"
         open(unit=11,file='config_files/config_data.txt',status='old')
         read(11,*)nc
         read(11,*)ne
         read(11,*)np
         close(unit=11)
		 allocate(read_cell_fate(nc))
         open(unit=12,file='config_files/cell_fate_data.txt',status='old')
         do n=1, nc
         	read(12,*)read_cell_fate(n)
         end do
         close(unit=12)
      end if
      nc_size=4*nc          !These are parameters for array size allocation from ScEM_0_input.f90
      ne_size=4*ne
      np_size=4*np

      ! allocate cell and element data arrays
      allocate(cells(nc_size))
      allocate(elements(ne_size))

      ! allocate center of mass, x_prev, and x_compare arrays
      allocate(xe_prev(ne_size,3)) ! x_prev records prior values of positions
      allocate(xe_compare(ne_size,3)) ! x_compare records values of positions at time of sector assignments

      ! allocate sector and pairs arrays
      allocate(head(nx,ny,nz))
      allocate(list(ne_size))
      allocate(pairs(np_size,2))
      !pairs_cortex can only be allocated after volume_calculate has been called because it relies on the Delaunay triangulation.

      if (flag_create.eq.1) then
         call scem_initial_create ! make new initial cell configuration
      else
         call scem_initial_exist ! read in data file of existing cell configuration
      end if

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

      if (flag_cortex.EQ.1) then
        !Find cortex elements
        call scem_cortex
      endif

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
