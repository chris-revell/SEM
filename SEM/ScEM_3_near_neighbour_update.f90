!CK Revell, April 2016
!Module to update element velocities according to standard near-neighbour morse potential interactions
!Contains subroutine scem_near_neighbour_update that performs local calculations to update the global velocities
!that are then passed on to other routines by scem_integrate.
module scem_3_near_neighbour_update

  use scem_1_potential
  use scem_1_types
  use scem_2_decouple_adhesion

contains

  subroutine scem_near_neighbour_update

    integer :: m,n,nn,k,kk
    integer :: bin,index_intra
    real*8  :: sep_sq,pot_deriv_interp1,pot_deriv_interp2,fadein_amp,r_s1,r_s2!,adhesion_factor_applied
    real*8, dimension(3) :: dx

    !Change adhesion magnitudes to account for local surface element density
    if (nc.LE.1.OR..NOT.intro) call scem_decouple_adhesion

    do m=1,np

      n=pairs(m,1)           !n and nn are the global labels of each element in the pair currently under consideration.
      nn=pairs(m,2)
      k=elements(n)%parent   !k and kk are the global labels of parent cells of n and nn
      kk=elements(nn)%parent

      !This section determines whether the two elements of the pair are in the same cell or different cells,
      !and sets an index to different values accordingly
      if (k.eq.kk) then
        index_intra=1     !Both elements n and nn are in the same cell
      else
        index_intra=2     !Elements are in different cells
      end if

      !Calculate the vector displacement separating elements
      dx(:)=elements(n)%position(:)-elements(nn)%position(:)
      sep_sq=dot_product(dx,dx)
      !Normalising dx by separation to give a unit vector in the direction between the two elements - THIS WAS NOT DONE IN THE ORIGINAL PROGRAM. WHY?
      dx(:)=dx(:)/sqrt(sep_sq)

      !Update element velocities according to interaction potentials with overdamped langevin dynamics
      if (sep_sq.le.r_interaction_max_sq) then !Separation between elements less than maximum range of interaction, so elements can interact.

        !This next section applies the standard morse potential between the two elements.
        fadein_amp = elements(n)%strength*elements(nn)%strength
        bin        = int(sep_sq*d_r_sq_recip)
        if (intro) then
          r_s1 = fadein_amp*intro_rel_strength(1,cells(k)%fate,cells(k)%fate,elements(n)%type,elements(nn)%type,index_intra)
          r_s2 = fadein_amp*intro_rel_strength(2,cells(k)%fate,cells(k)%fate,elements(n)%type,elements(nn)%type,index_intra)
        else
          r_s1 = fadein_amp*rel_strength(1,cells(k)%fate,cells(k)%fate,elements(n)%type,elements(nn)%type,index_intra)
          r_s2 = fadein_amp*rel_strength(2,cells(k)%fate,cells(k)%fate,elements(n)%type,elements(nn)%type,index_intra)
        endif

        !Calculate an adhesion factor to apply from the adhesion factors of the two cells in this pair, and the normalisation factor needed to recover natural number adhesion magnitudes.
        !The exact form of this calculation might need a little more thought and justification in future. Should it be the product of two factors, their mean, or the minimum of the two?
        if (index_intra.EQ.1.OR.elements(n)%type.EQ.1.OR.elements(nn)%type.EQ.1.OR.intro) then
          !Both elements are in the same cell, so no adhesion_factor should be applied to attractive component
          pot_deriv_interp1 = r_s1*(sep_sq*potential_deriv1(bin,1) + potential_deriv1(bin,2))
          pot_deriv_interp2 = r_s2*(sep_sq*potential_deriv2(bin,1) + potential_deriv2(bin,2))
        else
          !Elements are in different cells, and thus an adhesion_factor should be applied to attractive component
          pot_deriv_interp1 = r_s1*MIN(elements(n)%adhesion_factor,elements(nn)%adhesion_factor)&
            *(sep_sq*potential_deriv1(bin,1) + potential_deriv1(bin,2))
          pot_deriv_interp2 = r_s2*(sep_sq*potential_deriv2(bin,1) + potential_deriv2(bin,2))
        endif

        !Element velocities updated.
        elements(n)%velocity(:) = elements(n)%velocity(:)+dx(:)*(pot_deriv_interp1 + pot_deriv_interp2)
        elements(nn)%velocity(:)= elements(nn)%velocity(:)-dx(:)*(pot_deriv_interp1 + pot_deriv_interp2)
      endif
    end do

  end subroutine scem_near_neighbour_update
end module scem_3_near_neighbour_update
