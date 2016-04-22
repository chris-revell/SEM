! this module contains the short subroutine scem_inflexion which calculates
! the inflexion point of the potential, r_inflex, using the Newton-Raphson method

! T. J Newman, Tempe, July 2010

module scem_1_inflexion

  use scem_0_arrays
  use scem_0_input
  use scem_0_useful

  implicit none

  contains

    subroutine scem_inflexion

      real*8 :: x,delta_x,tol,u

      delta_x=10.0 ! arbitrary large value to exceed error bound on first iteration
      x=1.0*rho ! reasonable guess for solution
      u=exp(rho) ! convenient variable change
      tol=1.d-12 ! desired relative precision of inflexion point

      ! Newton-Raphson iteration
      do while (abs(delta_x/x).gt.tol)
         delta_x=((1.0-2*x)-u*(1.0-4*x)*exp(-x))/(2.0-u*(5.0-4*x)*exp(-x))
         x=x+delta_x
      end do

      r_inflex=r_equil*sqrt(x/rho) ! inflexion point in unscaled variables


!This section can probably be removed in future
!******************************************************************************
      ! write r_inflex to potential data file
!      open(unit=31,file=output_folder//'/system_data/potential_data',status='old',access='append')
!      write(31,*)'inflexion point = ',r_inflex
!      close(unit=31)
!******************************************************************************

    end subroutine scem_inflexion

end module scem_1_inflexion
