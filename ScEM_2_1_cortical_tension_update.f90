!Module containing subroutine to use Delaunay Triangulation output to establish a network of
!cortex element near neighbour pairs, and then to define and apply an interaction potential
!that acts between near neighbours in this network in order to cover the cell surface with
!a cortical tension.

module scem_2_1_cortical_tension_update

  use scem_0_input
  use scem_0_useful
  use scem_0_arrays
  use scem_1_types

  implicit none

contains

  subroutine scem_cortical_tension_update

    integer :: pair_counter
    integer :: other_element_in_pair1
    integer :: other_element_in_pair2
    integer :: is_in_flag
    integer :: c
    real    :: sep_sq
    real*8, dimension(3) :: dx

    pair_counter=0
    !Loop over all cells
    do c=1, nc
      !Loop over all cortex elements in cell c
      do i=1, cells(c)%cortex_elements(0)
        !Labels i of cortex elements in ltri() and cells()%triplets run from 1 to n where n is the
        !number of cortex elements. They can be converted back to global labels with cells()%cortex_elements(i)
        !The following loop is used to test all triplets j from 1 to cells(c)%triplet_count
        do j=1, cells(c)%triplet_count
          !The following loop is used to test all 3 elements in triplet j to see if any of them is the same as element i.
          !If k=1 gives an element that is the same as element i there is no need to test the other two.
          do k=1, 3
            if(cells(c)%triplets(k,j).EQ.i) then
              !If element cells(c)%triplets(k,j) is element i then element i is part of this triangle
              !Update pairs_cortex array with this element pair
              if(k.EQ.1) then
                other_element_in_pair1 = min(cells(c)%triplets(2,j),cells(c)%triplets(3,j)) !Use of max and min functions ensures that other_element_in_pair2 is the element with the higher global label of the two.
                other_element_in_pair2 = max(cells(c)%triplets(2,j),cells(c)%triplets(3,j))
              elseif(k.EQ.2) then
                other_element_in_pair1 = min(cells(c)%triplets(1,j),cells(c)%triplets(3,j))
                other_element_in_pair2 = max(cells(c)%triplets(1,j),cells(c)%triplets(3,j))
              else !k.EQ.3
                other_element_in_pair1 = min(cells(c)%triplets(1,j),cells(c)%triplets(2,j))
                other_element_in_pair2 = max(cells(c)%triplets(1,j),cells(c)%triplets(2,j))
              end if
              EXIT !No need to check remaining k values
            else
              CYCLE !Still need to check remaining k values
            endif
          enddo

          pair_counter = pair_counter + 1
          pairs_cortex(pair_counter,1) = other_element_in_pair1
          pairs_cortex(pair_counter,2) = other_element_in_pair2

          !Now have 2 elements that are in cortex pairs with element i
          !Remove redundancy by checking for pre-existing pairs in array before adding
          !First require a condition for when there are no pairs in the array yet.
!          if (pair_counter.EQ.0) then
!            pairs_cortex(1,1) = other_element_in_pair1
!            pairs_cortex(1,2) = other_element_in_pair2
!            pair_counter = pair_counter + 1
!          else
!            is_in_flag = 0 !Initiate is_in_flag=0. Set =1 if the newly found pair is seen to exist in pairs_cortex already
!            do l=1, pair_counter
!              if ((pairs_cortex(l,1).EQ.other_element_in_pair1).AND.(pairs_cortex(l,2).EQ.other_element_in_pair2)) then
                !In this case, the newly found pair already exists in pairs_cortex, so we set is_in_flag = 1
!                is_in_flag = 1
!                EXIT
!              else
!                CYCLE
!              endif
!            enddo
!            if (is_in_flag.EQ.1) then !pair is already in pairs_cortex, so do nothing and cycle to next triangle
!              CYCLE
!            else !Pair is not already in pairs_cortex, so we can add it
!              pairs_cortex(pair_counter,1) = other_element_in_pair1
!              pairs_cortex(pair_counter,2) = other_element_in_pair2
!              pair_counter = pair_counter + 1
!            endif
!          endif
        enddo !End loop over triangles from Delaunay triangulation
      enddo !End loop over cortex elements in cell
    enddo !End loop over cells
!    pair_counter=pair_counter-1 !Pair counter will always count 1 past the actual number of pairs, so for later use we subtract that 1 after the routine above
    !At this point we have evaluated the pairs_cortex(:,:) array to define the cortical network.

    !Now update velocities for all pairs in this network.
    do m=1,pair_counter
      n=pairs_cortex(m,1)           !n and nn are the global labels of each element in the pair currently under consideration.
      nn=pairs_cortex(m,2)
      k=elements(n)%parent   !k and kk are the global labels of parent cells of n and nn
      kk=elements(nn)%parent

      !Calculate the vector displacement separating elements
      dx(:)=elements(n)%position(:)-elements(nn)%position(:)
      sep_sq=dot_product(dx,dx)
      !Normalising dx by separation to give a unit vector in the direction between the two elements - THIS WAS NOT DONE IN THE ORIGINAL PROGRAM. WHY?
      dx(:)=dx(:)/sqrt(sep_sq)

      !Update element velocities according to interaction potentials with overdamped langevin dynamics
      !Constant force cortex_constant applied.
      elements(n)%velocity(:) = elements(n)%velocity(:)-dx(:)*cortex_constant
      elements(nn)%velocity(:)= elements(nn)%velocity(:)+dx(:)*cortex_constant
    end do

    end subroutine scem_cortical_tension_update
end module scem_2_1_cortical_tension_update
