! T. J Newman, Tempe, July 2010

module scem_5_initialize

  use scem_0_arrays
  use scem_0_input
  use scem_0_ran_array
  use scem_0_useful
  use scem_1_types
  use scem_2_com
  use scem_2_identity
  use scem_2_initial_create
  use scem_2_initial_exist
  use scem_2_output_system
  use scem_2_output_povray_elements
  use scem_2_output_povray_pairs
  use scem_2_output_povray_cell_positions
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
         open(unit=11,file='config_files/config_data',status='old')
         read(11,*)nc
         read(11,*)ne
         read(11,*)np
         close(unit=11)
		 allocate(read_cell_fate(nc))
         open(unit=12,file='config_files/cell_fate_data',status='old')
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

      ! open data file for element positions
      open(unit=21,file=output_folder//'/system_data/elements',status='unknown')
      ! open/write/close data file for initialization data check
      open(unit=22,file=output_folder//'/system_data/initialization_data',status='unknown')
      ! open data file for radius of gyration and center of mass
!      open(unit=23,file='radius_gyration',status='unknown')
      ! open data file to log relisting events
!      open(unit=24,file='relist_log',status='unknown')

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

      !Make all elements cortex elements
      !!!!!!!!!!!!!!!!!!!!
!      do i=1, nc
!        do j=1, cells(i)%c_elements(0)
!          cells(i)%cortex_elements(0) = cells(i)%c_elements(0)
!          cells(i)%cortex_elements(j) = cells(i)%c_elements(j)
!        enddo
!      enddo
      !!!!!!!!!!!!!!!!!!!

      ! write/close data file for initialization data check
      write(22,*)'number of elements = ',ne
      write(22,*)'number of pairs = ',np
      write(22,*)'linear size of sector array = ',nx
      write(22,*)'radius of first cell = ',real(r_cell)
      write(22,*)'linear sector size = ',real(sector_size)
      write(22,*)'time = ',real(time)
      write(22,*)'center of mass = ',real(cells(1)%position(:))
      write(22,*)'frac_interaction_max = ',frac_interaction_max
!      write(22,*)'boundary radius = ',r_boundary
      write(22,*) "Interaction magnitudes"
  	  write(22,*) rel_strength(1,1,1,1,1), "Intra-cellular Epiblast cytoplasm-epiblast cytoplasm"
  	  write(22,*) rel_strength(1,1,1,2,1), "Intra-cellular Epiblast cytoplasm-epiblast cortex"
  	  write(22,*) rel_strength(1,1,2,2,1), "Intra-cellular Epiblast cortex-epiblast cortex"
  	  write(22,*) rel_strength(1,2,1,1,1), "Intra-cellular Epiblast cytoplasm-hypoblast cytoplasm"
  	  write(22,*) rel_strength(1,2,1,2,1), "Intra-cellular Epiblast cytoplasm-hypoblast cortex"
  	  write(22,*) rel_strength(1,2,2,2,1), "Intra-cellular Epiblast cortex-hypoblast cortex"
  	  write(22,*) rel_strength(2,2,1,1,1), "Intra-cellular Hypoblast cytoplasm-hypoblast cytoplasm"
  	  write(22,*) rel_strength(2,2,1,2,1), "Intra-cellular Hypoblast cytoplasm-hypoblast cortex"
  	  write(22,*) rel_strength(2,2,2,2,1), "Intra-cellular Hypoblast cortex-hypoblast cortex"
  	  write(22,*)
  	  write(22,*) rel_strength(1,1,1,1,2), "Inter-cellular Epiblast cytoplasm-epiblast cytoplasm"
  	  write(22,*) rel_strength(1,1,1,2,2), "Inter-cellular Epiblast cytoplasm-epiblast cortex"
  	  write(22,*) rel_strength(1,1,2,2,2), "Inter-cellular Epiblast cortex-epiblast cortex"
  	  write(22,*) rel_strength(1,2,1,1,2), "Inter-cellular Epiblast cytoplasm-hypoblast cytoplasm"
  	  write(22,*) rel_strength(1,2,1,2,2), "Inter-cellular Epiblast cytoplasm-hypoblast cortex"
  	  write(22,*) rel_strength(1,2,2,2,1), "Inter-cellular Epiblast cortex-hypoblast cortex"
  	  write(22,*) rel_strength(2,2,1,1,2), "Inter-cellular Hypoblast cytoplasm-hypoblast cytoplasm"
  	  write(22,*) rel_strength(2,2,1,2,2), "Inter-cellular Hypoblast cytoplasm-hypoblast cortex"
  	  write(22,*) rel_strength(2,2,2,2,2), "Inter-cellular Hypoblast cortex-hypoblast cortex"
      close(unit=22)

	    ! Calculate initial cell volumes
      call volume_calculate

      ! write initial system data to file
      call scem_output_system

      ! Write element data to files in povray format
      if (flag_povray_elements.EQ.1) then
        call scem_output_povray_elements
      endif
      ! Write pair data to files in povray format
      if (flag_povray_pairs.EQ.1) then
        call scem_output_povray_pairs
      endif
      !Write cell position data to file in povray format
      if (flag_povray_cells.EQ.1) then
        call scem_output_povray_cell_positions
      endif

    end subroutine scem_initialize

end module scem_5_initialize
