! T. J Newman, Tempe, July 2010

module scem_2_integrate

  use scem_0_arrays
  use scem_0_input
  use scem_0_ran_array
  use scem_0_useful
  use scem_1_potential
  use scem_1_potential_2
  use scem_1_types
  use scem_2_background
  use scem_2_DIT

  implicit none


  contains

    subroutine scem_integrate

      real*8, dimension(3) :: dx
      real*8 :: dz,sep_sq,pot_deriv_interp,fadein_amp,r_s
      real*8 :: element_radius_sq
      real*8 :: element_radius
      integer :: bin,index_intra

      if (flag_DIT.EQ.1) then
        call scem_dit
      endif

      do m=1,np
         n=pairs(m,1)     !Label of each cell in pair
         nn=pairs(m,2)
         k=elements(n)%parent     !Label of parent cell for each element in pair
         kk=elements(nn)%parent
         !This section determines whether the two elements of the pair are in the same cell or different cells, and sets an index to different values accordingly
         if (k.eq.kk) then
            index_intra=1     !Both elements of pair are in the same cell
         else
            index_intra=2     !Elements are in different cells
         end if
         dx(:)=elements(n)%position(:)-elements(nn)%position(:)     !Vector displacement separating elements
         sep_sq=dot_product(dx,dx)
         dx(:)=dx(:)/sqrt(sep_sq)       !Normalising dx by separation to give a unit vector - THIS WAS NOT DONE IN THE ORIGINAL PROGRAM. WHY?


       	!Update element velocities according to interaction potentials with overdamped langevin dynamics
     	  if (sep_sq.le.r_interaction_max_sq) then !Separation between elements less than maximum range of interaction, so elements can interact.

          !This next section applies the standard morse potential between the two elements.
          fadein_amp  = elements(n)%strength*elements(nn)%strength
     	 	  bin         = int(sep_sq*d_r_sq_recip)
          r_s         = fadein_amp*rel_strength(cells(k)%fate,cells(kk)%fate,elements(n)%type,elements(nn)%type,index_intra)
       		pot_deriv_interp        = r_s*(sep_sq*potential_deriv(bin,1) + potential_deriv(bin,2))
          !Element velocities updated.
          elements(n)%velocity(:) = elements(n)%velocity(:)+dx(:)*pot_deriv_interp
        	elements(nn)%velocity(:)= elements(nn)%velocity(:)-dx(:)*pot_deriv_interp

          !This next section applies an additional cortical tension term to the velocity calculation.
          if (index_intra.EQ.1) then  !If both elements are in the same cell and...
            if (elements(n)%type.EQ.2.AND.elements(nn)%type.EQ.2) then !If both elements are cortex elements, type 2, then we apply the cortical tension term.
              pot_deriv_interp        = r_s*elements(n)%DIT_factor*elements(nn)%DIT_factor*&
                    (sep_sq*potential_deriv_2(bin,1) + potential_deriv_2(bin,2))
        		  elements(n)%velocity(:) = elements(n)%velocity(:)+dx(:)*pot_deriv_interp
            	elements(nn)%velocity(:)= elements(nn)%velocity(:)-dx(:)*pot_deriv_interp
            endif
     	    end if
        endif
      end do

      !Update element velocities according to background potential.
      if (flag_background.GT.0) then
        call scem_background
      end if

    end subroutine scem_integrate

end module scem_2_integrate
