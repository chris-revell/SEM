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
    integer :: c
    real    :: sep_sq
    real*8, dimension(3) :: dx

    !Allocate the size of the pairs_cortex array by summing over 3 times the number of delaunay triangles in each cell.
    if (allocated(pairs_cortex)) deallocate(pairs_cortex)
    np_cortex = 0
    do i=1, nc
      np_cortex = np_cortex + 3*cells(i)%triplet_count
    enddo
    allocate(pairs_cortex(np_cortex,3))

    !Default setting for the 3rd column of the 2nd dimension of pairs_cortex, updated later in scem_DIT
    pairs_cortex(:,3)=1

    pair_counter=0
    !Loop over all cells
    do c=1, nc
      !Loop over all Delaunay triplets in cell c
      do j=1, cells(c)%triplet_count
        !Each edge of the Delaunay triangle adds a pair to the pairs_cortex array
        pair_counter = pair_counter+1
        pairs_cortex(pair_counter,1) = cells(c)%triplets(1,j)   !Use min(cells(c)%triplets(1,j),cells(c)%triplets(2,j)) if we want to ensure that the lowest label comes first.
        pairs_cortex(pair_counter,2) = cells(c)%triplets(2,j)   !Use max(cells(c)%triplets(1,j),cells(c)%triplets(2,j)) if we want to ensure that the highest label comes second.

        pair_counter = pair_counter+1
        pairs_cortex(pair_counter,1) = cells(c)%triplets(2,j)
        pairs_cortex(pair_counter,2) = cells(c)%triplets(3,j)

        pair_counter = pair_counter+1
        pairs_cortex(pair_counter,1) = cells(c)%triplets(3,j)
        pairs_cortex(pair_counter,2) = cells(c)%triplets(1,j)
      enddo !End loop over triangles
    enddo !End loop over cells


    !Now update velocities for all pairs in this network.
    do m=1,pair_counter
      n=pairs_cortex(m,1)           !n and nn are the global labels of each element in the pair currently under consideration.
      nn=pairs_cortex(m,2)

      !Calculate the vector displacement separating elements

      dx(:)=elements(n)%position(:)-elements(nn)%position(:)
      sep_sq=dot_product(dx,dx)
      !Normalising dx by separation to give a unit vector in the direction between the two elements - THIS WAS NOT DONE IN THE ORIGINAL PROGRAM. WHY?
      dx(:)=dx(:)/sqrt(sep_sq)

      !Update element velocities according to interaction potentials with overdamped langevin dynamics
      !Constant force cortex_constant applied.
      elements(n)%velocity(:) = elements(n)%velocity(:) - dx(:)*cortex_constant*pairs_cortex(m,3)
      elements(nn)%velocity(:)= elements(nn)%velocity(:)+ dx(:)*cortex_constant*pairs_cortex(m,3)
    end do

  end subroutine scem_cortical_tension_update
end module scem_2_1_cortical_tension_update
