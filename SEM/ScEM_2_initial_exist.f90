! T. J Newman, Tempe, July 2010

module scem_2_initial_exist

  use scem_0_arrays
  use scem_0_input
  use scem_0_useful
  use scem_1_types

  implicit none

  contains

    subroutine scem_initial_exist

      integer :: epi_counter
      integer :: hypo_counter

      WRITE(*,*) "READ"

      !Read number of cells, elements and pairs from config_data.txt
      open(unit=11,file='config_files/config_data.txt',status='old')
      read(11,*)nc
      read(11,*)ne
      read(11,*)np
      close(unit=11)
      nc_size=4*nc
      ne_size=4*ne
      np_size=4*np
      ! allocate cell and element data arrays
      allocate(cells(nc_size))
      allocate(elements(ne_size))

      !Set cell fates from file or randomly
      if (flag_randomise.EQ.1) then
        !Set fates for initial cells randomly
        epi_counter = 0
        hypo_counter= 0
        do n=1, nc
          CALL RANDOM_NUMBER(fate_decider)
          !Ensure that the number of epiblast or hypoblast cannot exceed half the total number of cells (or half+1 if nc is odd)
          if (epi_counter.GE.(nc/2)) then
            cells(n)%fate = 2
            hypo_counter = hypo_counter+1
          elseif (hypo_counter.GE.(nc/2)) then
            cells(n)%fate = 1
            epi_counter = epi_counter+1
          elseif (fate_decider.GE.0.5) then
            cells(n)%fate = 1
            epi_counter = epi_counter+1
          else
            cells(n)%fate = 2
            hypo_counter = hypo_counter+1
          endif
        enddo
        print*, "epi_counter", epi_counter
        print*, "hypo_counter", hypo_counter
      else
        !Read fates for initial cells from file cell_fate_data.txt
        open(unit=12,file='config_files/cell_fate_data.txt',status='old')
        do n=1, nc
         read(12,*) cells(n)%fate
        end do
        close(unit=12)
      endif

      !Set cell labels
      do n=1,nc
        cells(n)%label = n
      enddo

      !Read element positions and parents from elements_config.txt, and initialise other element parameters accordingly.
      open(unit=12,file='config_files/elements_config.txt',status='old')
      do n=1,ne
         read(12,*)elements(n)%position(:), elements(n)%parent
         elements(n)%label=n ! Define new element label
         elements(n)%type=2 ! Default element type is 2, bulk/cytoplasm. Cortex elements allocated later.
         elements(n)%stage=1 ! Define status of element to be steady-state.
         elements(n)%age=establishment_time ! initialize age of new element
         elements(n)%strength=1.0 ! weighting for potential interactions
         elements(n)%velocity(:)=0.0 ! initialize velocity of new element
      end do
      close(unit=12)

    end subroutine scem_initial_exist

end module scem_2_initial_exist
