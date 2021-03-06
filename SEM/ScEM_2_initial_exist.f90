! T. J Newman, Tempe, July 2010

module scem_2_initial_exist

  use scem_0_arrays
  use scem_0_input

  use scem_1_types

  implicit none

  contains

    subroutine scem_initial_exist

      integer :: n
      integer :: epi_counter
      integer :: pre_counter
      real*8  :: fate_decider
      logical :: fatesnotbalanced

      intro = .FALSE.

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
      nc_initial = nc

      !Set cell fates from file or randomly
      if (flag_randomise.EQ.1) then
        !Set fates for initial cells randomly
        fatesnotbalanced = .TRUE.
        do while (fatesnotbalanced)
          epi_counter = 0
          pre_counter = 0
          do n=1, nc
            CALL RANDOM_NUMBER(fate_decider)
            if (fate_decider.GE.0.5) then
              cells(n)%fate = 1
              epi_counter = epi_counter+1
            else
              cells(n)%fate = 2
              pre_counter = pre_counter+1
            endif
          enddo
          if (MOD(nc,2).EQ.0) then
            if (epi_counter.EQ.pre_counter) fatesnotbalanced = .FALSE.
          else
            if (ABS(epi_counter-pre_counter).EQ.1) fatesnotbalanced = .FALSE.
          endif
        enddo
      else
        !Read fates for initial cells from file cell_fate_data.txt
        open(unit=12,file='config_files/cell_fate_data.txt',status='old')
        epi_counter = 0
        pre_counter = 0
        do n=1, nc
         read(12,*) cells(n)%fate
         if (cells(n)%fate.EQ.1) then
           epi_counter = epi_counter + 1
         else
           pre_counter = pre_counter +1
         endif
        end do
        close(unit=12)
      endif
      write(*,'(A18,I2,A7)') "Initial system of ",nc," cells."
      write(*,'(A29,I2)') "Initial number of epiblasts: ", epi_counter
      write(*,'(A30,I2)') "Initial number of hypoblasts: ", pre_counter

      !Set cell labels
      do n=1,nc
        cells(n)%label = n
        cells(n)%age=establishment_time
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
