! this module contains the subroutine scem_potential, which creates a table for efficient velocity evaluation
! the table potential_deriv is constructed, and this, appropriately scaled, will be used for all interactions
! see subroutine scem_integration for implementation

! T. J Newman, Tempe, July 2010

module scem_1_potential_2

  use scem_0_arrays
  use scem_0_input
  use scem_0_useful

  implicit none

  contains

    subroutine scem_potential_2
      real*8 :: sep_sq,factor,dist,pot
      real*8, allocatable, dimension(:) :: r_sq_table
      real*8, allocatable, dimension(:) :: pot_deriv_table

      allocate(potential_deriv_2(0:n_bins-1,2)) ! potential_deriv is an interpolation table for (1/r)dV/dr            Note n_bins is the number of elements into which the max distance of potential is divided (I think)
      allocate(pot_deriv_table(0:n_bins)) ! pot_deriv_table is a local array containing (1/r)dV/dr
      allocate(r_sq_table(0:n_bins)) ! r_sq_table is a local array containing tabulated values of r^2

      ! loop over table bins
      do j=0,n_bins
         sep_sq             = j*d_r_sq ! calculate r^2
         r_sq_table(j)      = sep_sq ! fill r_sq_table
         pot_deriv_table(j) = -cortex_constant ! fill pot_deriv_table
      end do

      ! loop over bins and use linear interpolation to fill potential_deriv
      do j=0,n_bins-1
         potential_deriv_2(j,1)=(pot_deriv_table(j+1)-pot_deriv_table(j))*d_r_sq_recip
         potential_deriv_2(j,2)=(pot_deriv_table(j)*r_sq_table(j+1)-pot_deriv_table(j+1)*r_sq_table(j))*d_r_sq_recip
      end do

      ! rescale force by damping_element to retrieve (what ultimately will be) a velocity
      ! note, for efficiency, this quantity needs to be multiplied by a position vector to yield a true velocity
      ! see scem_2_integration for implementation
      potential_deriv_2=potential_deriv_2/damping_element

      ! deallocate local arrays
      deallocate(pot_deriv_table)
      deallocate(r_sq_table)

      ! write useful potential data to file
      open(unit=31,file=output_folder//'/system_data/potential_data',status='unknown')
      write(31,*) ""
      write(31,*)'Potential 2 : '
      write(31,*)'maximum range = ',r_interaction_max
      write(31,*)'cortex_constant',cortex_constant
      close(unit=31)

      ! write functional form of potential to file
!      open(unit=?,file=output_folder//'/system_data/potential_function_2',status='unknown')
!      do j=0,n_bins
!         dist=((j+0.0)/n_bins)*r_interaction_max
!         pot=pot_min*(exp(2*rho*(1.0-dist**2/r_equil_sq))-2*exp(rho*(1.0-dist**2/r_equil_sq)))
!         write(?,*)dist,pot
!      end do
!      close(unit=32)

    end subroutine scem_potential_2

end module scem_1_potential_2
