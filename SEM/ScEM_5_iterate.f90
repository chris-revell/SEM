! T. J Newman, Tempe, July 2010

module scem_5_iterate

  use scem_0_arrays
  use scem_0_input
  use scem_0_useful
  use scem_1_types
  use scem_2_ageing
  use scem_2_com
  use scem_2_diffusion
  use scem_2_division
  use scem_2_flag_relist
  use scem_2_growth
  use scem_2_integrate
  use scem_2_output_system
  use scem_2_output_povray
  use scem_2_pairs
  use scem_2_relist
  use scem_2_resize
  use scem_4_cortex
  use volume_calculate_module
  use volume_conserve_module

  implicit none

  contains

    subroutine scem_iterate

      ! iterate system for pre-defined time interval
      do while (time.lt.time_max)

        time=time+dt ! increment time

        forall(n=1:ne) xe_prev(n,:)=elements(n)%position(:) ! xe_prev records prior positions of elements

        ! implement 2nd order Runge-Kutta
        call scem_integrate ! first integration and increment by half a step
        forall(n=1:ne) elements(n)%position(:)=elements(n)%position(:)+0.5*dt*elements(n)%velocity(:)
        forall(n=1:ne) elements(n)%velocity(:)=0.0
        call scem_integrate ! second integration and increment by a full step
        forall(n=1:ne) elements(n)%position(:)=xe_prev(n,:)+dt*elements(n)%velocity(:)

        ! element diffusion
        if (flag_diffusion.eq.1) then
          call scem_diffusion
        end if

        !Conserve cell volumes
        do n=1, nc
          if (flag_conserve.eq.1) then
            call volume_conserve(n)
          endif
          deallocate(cells(n)%triplets)
        end do

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
        if (flag_growth.eq.1) then
          call scem_growth
        end if

        ! cell division
        if (flag_division.eq.1) then
          call scem_division
        end if

        ! check for resizing of (element,cell,sector) arrays
        if (flag_growth.eq.1) then
          call scem_resize
        end if

        ! re-calculate center of mass and radius of gyration of each cell
        call scem_com

        !Determine which elements are now cortex elements
        call scem_cortex

        !Calculate cell volumes
        call volume_calculate

        !Outputting data to file
        !Only output data at intervals of time_out_1.
        !time_out_1 = cell_cycle_time/10.0 and is the time interval between data outputs
        if (mod(time,(time_out_1)).lt.dt) then
          !Increment n_snapshots to keep track of how many outputs there have been
          n_snapshots=n_snapshots+1
          ! write system data to files
          call scem_output_system
          ! Write element data to files in povray format
          if (flag_povray.EQ.1) then
            call scem_output_povray
          endif
        end if

        forall(n=1:ne) elements(n)%velocity(:)=0.0

      end do

    end subroutine scem_iterate

end module scem_5_iterate
