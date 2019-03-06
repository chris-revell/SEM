!Module containing subroutine to use Delaunay Triangulation output to establish a network of
!cortex element near neighbour pairs, and then to define and apply an interaction potential
!that acts between near neighbours in this network in order to cover the cell surface with
!a cortical tension.

module scem_3_cortical_tension_update

  use scem_0_input
  use scem_0_arrays
  use scem_1_types

  implicit none

contains

  subroutine scem_cortical_tension_update

    integer :: m,n,nn
    real    :: sep_sq,bleb_factor_n,bleb_factor_nn
    real*8, dimension(3) :: dx

    !Now update velocities for all pairs in this network.
    do m=1,np_cortex
      n=pairs_cortex(m)%label1           !n and nn are the global labels of each element in the pair currently under consideration.
      nn=pairs_cortex(m)%label2

      !Calculate the vector displacement separating elements
      dx(:)=elements(n)%position(:)-elements(nn)%position(:)
      sep_sq=dot_product(dx,dx)
      !Normalising dx by separation to give a unit vector in the direction between the two elements - THIS WAS NOT DONE IN THE ORIGINAL PROGRAM. WHY?
      dx(:)=dx(:)/sqrt(sep_sq)

      !Update element velocities according to interaction potentials with overdamped langevin dynamics
      !Constant force cortex_constant1 or cortex_constant2 applied depending on fate of cell that contains pair.
      if(cells(elements(n)%parent)%label.EQ.1) then
        elements(n)%velocity(:) = elements(n)%velocity(:) - dx(:)*cortex_constant1*pairs_cortex(m)%cortex_factor
        elements(nn)%velocity(:)= elements(nn)%velocity(:)+ dx(:)*cortex_constant1*pairs_cortex(m)%cortex_factor
      else
        bleb_factor_n  = 1.0
        bleb_factor_nn = 1.0
        if (flag_pre_blebbing.EQ.1.AND..NOT.intro) then
          if (elements(n)%DIT_factor.EQ.0)  bleb_factor_n = 1.0 + bleb_amp*SIN(10*2.0*pi*elements(n)%age/cell_cycle_time)
          if (elements(nn)%DIT_factor.EQ.0) bleb_factor_nn= 1.0 + bleb_amp*SIN(10*2.0*pi*elements(nn)%age/cell_cycle_time)
        endif
        elements(n)%velocity(:) = elements(n)%velocity(:) - dx(:)*cortex_constant2*pairs_cortex(m)%cortex_factor*bleb_factor_n
        elements(nn)%velocity(:)= elements(nn)%velocity(:)+ dx(:)*cortex_constant2*pairs_cortex(m)%cortex_factor*bleb_factor_nn
      endif
    end do

  end subroutine scem_cortical_tension_update
end module scem_3_cortical_tension_update
