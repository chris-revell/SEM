! T. J Newman, Tempe, July 2010

module scem_5_iterate

  use scem_0_arrays
  use scem_0_input
  use scem_1_types
  use scem_2_ageing
  use scem_2_com
  use scem_2_diffusion
  use scem_2_division
  use scem_2_flag_relist
  use scem_2_growth
  use scem_2_output_povray
  use scem_2_pairs
  use scem_2_relist
  use scem_2_resize
  use scem_4_cortex
  use scem_1_volume_calculate
  use scem_1_volume_conserve
  use scem_4_integrate
  use scem_4_output_system

  implicit none

contains

  subroutine scem_iterate

    integer :: n
    integer :: epi_counter
    integer :: hypo_counter
    real*8  :: fate_decider
    logical :: fatesnotbalanced

    ! iterate system for pre-defined time interval
    do while (time.LT.time_max)

      if (intro.AND.nc.GE.nc_initial) then
        write(*,'(A20,I2,A41)') "Grew intro system to",nc_initial," cells. Initiating simulation parameters."
        intro = .FALSE.
        !Set fates for initial cells randomly
        fatesnotbalanced = .TRUE.
        do while (fatesnotbalanced)
          epi_counter = 0
          hypo_counter= 0
          do n=1, nc
            CALL RANDOM_NUMBER(fate_decider)
            if (fate_decider.GE.0.5) then
              cells(n)%fate = 1
              epi_counter = epi_counter+1
            else
              cells(n)%fate = 2
              hypo_counter = hypo_counter+1
            endif
          enddo
          if (MOD(nc,2).EQ.0) then
            if (epi_counter.EQ.hypo_counter) fatesnotbalanced = .FALSE.
          else
            if (ABS(epi_counter-hypo_counter).EQ.1) fatesnotbalanced = .FALSE.
          endif
        enddo
        write(*,'(A29,I2)') "Initial number of epiblasts: ", epi_counter
        write(*,'(A30,I2)') "Initial number of hypoblasts: ", hypo_counter
        call scem_output_system
        if (flag_povray.EQ.1) call scem_output_povray
      endif

      if (.NOT.intro) time=time+dt ! increment time

      forall(n=1:ne) xe_prev(n,:)=elements(n)%position(:) ! xe_prev records prior positions of elements

      ! implement 2nd order Runge-Kutta
      call scem_integrate ! first integration and increment by half a step
      forall(n=1:ne) elements(n)%position(:)=elements(n)%position(:)+0.5*dt*elements(n)%velocity(:)
      forall(n=1:ne) elements(n)%velocity(:)=0.0
      call scem_integrate ! second integration and increment by a full step
      forall(n=1:ne) elements(n)%position(:)=xe_prev(n,:)+dt*elements(n)%velocity(:)

      ! element diffusion
      if (flag_diffusion.eq.1) call scem_diffusion

      !Conserve cell volumes
      if (flag_conserve.eq.1) call scem_volume_conserve

      ! determine sector relist flag
      call scem_flag_relist
      ! if sector relist flag has been triggered, reconstruct sector tables and pair array
      if (flag_relist.eq.1) then
        ! re-create sector tables
        call scem_relist(1)
        ! locate element pairs
        call scem_pairs
      end if

      ! element and cell age updates
      call scem_ageing

      ! cell growth
      if (flag_growth.eq.1) call scem_growth

      ! cell division
      if (flag_division.eq.1) call scem_division

      ! check for resizing of (element,cell,sector) arrays
      if (flag_growth.eq.1) call scem_resize

      ! re-calculate center of mass and radius of gyration of each cell
      call scem_com

      !Determine which elements are now cortex elements
      call scem_cortex

      !Calculate cell volumes
      call scem_volume_calculate

      !Outputting data to file at intervals of output_interval.
      if (mod(time,(output_interval)).LT.dt.AND..NOT.intro) then
        n_snapshots=n_snapshots+1
        call scem_output_system
        if (flag_povray.EQ.1) call scem_output_povray
      end if

      forall(n=1:ne) elements(n)%velocity(:)=0.0

    end do

  end subroutine scem_iterate

end module scem_5_iterate
