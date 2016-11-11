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
  use scem_4_output_system
  use scem_2_output_povray
  use scem_2_pairs
  use scem_2_relist
  use scem_2_resize
  use scem_4_cortex
  use scem_4_volume_calculate
  use scem_4_volume_conserve

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
        if (flag_conserve.eq.1) then
          call scem_volume_conserve
        endif

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
        if (flag_conserve.EQ.1.OR.flag_volume_output.EQ.1) then
          call scem_volume_calculate
        endif

        !Outputting data to file
        !Only output measurement data at intervals of time_out_2
        if (mod(time,(time_out_2)).lt.dt) then
          ! write system data to files
          call scem_output_system
        endif

        !Only graphical output data at intervals of time_out_1.
        if (mod(time,(time_out_1)).lt.dt) then
          !Calculate time expired so far
          call SYSTEM_CLOCK(current_time)
          total_system_time = (current_time-start_time)/count_rate
          !Write system progress update to the command line.
          write(*,*) real(time),total_system_time,ne,nc,np,n_snapshots !Old version: real(time),ne,nc,ne_size,nc_size,np,np_size,nx,ny,nz,n_snapshots

          !Increment n_snapshots to keep track of how many outputs there have been
          n_snapshots=n_snapshots+1
          ! Write element data to files in povray format
          if (flag_povray.EQ.1) then
            call scem_output_povray
          endif
        end if

        forall(n=1:ne) elements(n)%velocity(:)=0.0

      end do

    end subroutine scem_iterate

end module scem_5_iterate
