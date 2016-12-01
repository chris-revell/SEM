! this module contains the subroutine scem_potential, which creates a table for efficient velocity evaluation
! the table potential_deriv2 is constructed, and this, appropriately scaled, will be used for all interactions
! see subroutine scem_integration for implementation

! T. J Newman, Tempe, July 2010

module scem_1_potential

  use scem_0_arrays
  use scem_0_input
  use omp_lib

  implicit none

  contains

    subroutine scem_potential

      integer :: j
      real*8  :: sep_sq,factor
      real*8, allocatable, dimension(:) :: r_sq_table
      real*8, allocatable, dimension(:) :: pot_deriv_table

      allocate(potential_deriv1(0:n_bins-1,2)) ! potential_deriv1 is an interpolation table for (1/r)dV_1/dr    Note n_bins is the number of length sections into which the max distance of potential is divided
      allocate(potential_deriv2(0:n_bins-1,2)) ! potential_deriv2 is an interpolation table for (1/r)dV_2/dr
      allocate(pot_deriv_table(0:n_bins)) ! pot_deriv_table is a local array containing (1/r)dV/dr
      allocate(r_sq_table(0:n_bins)) ! r_sq_table is a local array containing tabulated values of r^2

!For attractive potential
      !$omp parallel &
      !$omp shared (n_bins,r_sq_table,rho,r_equil_sq,force_amplitude,pot_deriv_table,d_r_sq) &
      !$omp private (j,sep_sq,factor)
      !$omp do
      do j=0,n_bins
         sep_sq=j*d_r_sq                               ! calculate r^2
         r_sq_table(j)=sep_sq                          ! fill r_sq_table
         factor=exp(rho*(1.0-sep_sq/r_equil_sq))       ! a useful factor for potential evaluation
         pot_deriv_table(j)=-force_amplitude*factor ! This is the force applied by the potential.
      end do
      !$omp end do
      !$omp end parallel

      ! loop over bins and use linear interpolation to fill potential_deriv2
      ! These values are not forces themselves but are used later in a linear interpolation to find forces.
      !$omp parallel &
      !$omp shared (n_bins,pot_deriv_table,d_r_sq_recip,r_sq_table,potential_deriv1) &
      !$omp private (j)
      !$omp do
      do j=0,n_bins-1
         potential_deriv1(j,1)=(pot_deriv_table(j+1)-pot_deriv_table(j))*d_r_sq_recip
         potential_deriv1(j,2)=(pot_deriv_table(j)*r_sq_table(j+1)-pot_deriv_table(j+1)*r_sq_table(j))*d_r_sq_recip
      end do
      !$omp end do
      !$omp end parallel

      ! rescale force by damping_element to retrieve (what ultimately will be) a velocity
      ! note, for efficiency, this quantity needs to be multiplied by a position vector to yield a true velocity
      ! see scem_2_integration for implementation
      potential_deriv1=potential_deriv1/damping_element

!For repulsive potential, same as above but with different formula for pot_deriv_table
      !$omp parallel &
      !$omp shared (n_bins,r_sq_table,rho,r_equil_sq,force_amplitude,pot_deriv_table,d_r_sq) &
      !$omp private (j,sep_sq,factor)
      !$omp do
      do j=0,n_bins
         sep_sq=j*d_r_sq                               ! calculate r^2
         r_sq_table(j)=sep_sq                          ! fill r_sq_table
         factor=exp(rho*(1.0-sep_sq/r_equil_sq))       ! a useful factor for potential evaluation
         pot_deriv_table(j)=force_amplitude*factor**2 ! This is the force applied by the potential.
      end do
      !$omp end do
      !$omp end parallel

      !$omp parallel &
      !$omp shared (n_bins,pot_deriv_table,d_r_sq_recip,r_sq_table,potential_deriv1) &
      !$omp private (j)
      !$omp do
      do j=0,n_bins-1
         potential_deriv2(j,1)=(pot_deriv_table(j+1)-pot_deriv_table(j))*d_r_sq_recip
         potential_deriv2(j,2)=(pot_deriv_table(j)*r_sq_table(j+1)-pot_deriv_table(j+1)*r_sq_table(j))*d_r_sq_recip
      end do
      !$omp end do
      !$omp end parallel
      potential_deriv2=potential_deriv2/damping_element

      ! deallocate local arrays
      deallocate(pot_deriv_table)
      deallocate(r_sq_table)

    end subroutine scem_potential

end module scem_1_potential
