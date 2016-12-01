!Module containing subroutine to use Delaunay Triangulation output to establish a network of
!cortex element near neighbour pairs, and then to define and apply an interaction potential
!that acts between near neighbours in this network in order to cover the cell surface with
!a cortical tension.

module scem_3_cortical_tension_update

  use scem_0_input
  use scem_0_arrays
  use scem_1_types
  use scem_2_DIT

  implicit none

contains

  subroutine scem_cortical_tension_update

    integer :: pair_counter
    integer :: c,i,j,m,n,nn
    real    :: sep_sq
    real*8, dimension(3) :: dx

    !Allocate the size of the pairs_cortex array by summing over 3 times the number of delaunay triangles in each cell.
    if (allocated(pairs_cortex)) deallocate(pairs_cortex)
    np_cortex = 0
    do i=1, nc
      np_cortex = np_cortex + 3*cells(i)%triplet_count
    enddo
    allocate(pairs_cortex(np_cortex))

    pair_counter=0
    !Fill pairs_cortex(i)%label1 and pairs_cortex(i)%label2 with labels of elements in cortex pairs
    !Loop over all cells
    do c=1, nc
      !Loop over all Delaunay triplets in cell c
      !$omp parallel &
      !$omp shared (nc,cells,pairs_cortex,c) &
      !$omp private (j)
      !$omp do reduction (+ : pair_counter)
      do j=1, cells(c)%triplet_count
        !Each edge of the Delaunay triangle adds a pair to the pairs_cortex array
        pair_counter = pair_counter+1
        pairs_cortex(pair_counter)%label1 = cells(c)%triplets(1,j)   !Use min(cells(c)%triplets(1,j),cells(c)%triplets(2,j)) if we want to ensure that the lowest label comes first.
        pairs_cortex(pair_counter)%label2 = cells(c)%triplets(2,j)   !Use max(cells(c)%triplets(1,j),cells(c)%triplets(2,j)) if we want to ensure that the highest label comes second.

        pair_counter = pair_counter+1
        pairs_cortex(pair_counter)%label1 = cells(c)%triplets(2,j)
        pairs_cortex(pair_counter)%label2 = cells(c)%triplets(3,j)

        pair_counter = pair_counter+1
        pairs_cortex(pair_counter)%label1 = cells(c)%triplets(3,j)
        pairs_cortex(pair_counter)%label2 = cells(c)%triplets(1,j)
      enddo !End loop over triangles
      !$omp end do
      !$omp end parallel
    enddo !End loop over cells

    !Now that cortex network has been established in pairs_cortex(i,1)
    !and pairs_cortex(i,2), calculate pairs_cortex(i,3) values
    if (.NOT.intro) call scem_dit

    !Now update velocities for all pairs in this network.
    !$omp parallel &
    !$omp shared (pair_counter,pairs_cortex,elements,cells,intro) &
    !$omp private (m,n,nn,dx,sep_sq)
    !$omp do
    do m=1,pair_counter
      n=pairs_cortex(m)%label1           !n and nn are the global labels of each element in the pair currently under consideration.
      nn=pairs_cortex(m)%label2

      !Calculate the vector displacement separating elements
      dx(:)=elements(n)%position(:)-elements(nn)%position(:)
      sep_sq=dot_product(dx,dx)
      !Normalising dx by separation to give a unit vector in the direction between the two elements - THIS WAS NOT DONE IN THE ORIGINAL PROGRAM. WHY?
      dx(:)=dx(:)/sqrt(sep_sq)

      !Update element velocities according to interaction potentials with overdamped langevin dynamics
      !Constant force cortex_constant1 or cortex_constant2 applied depending on fate of cell that contains pair.
      if (intro) then
        elements(n)%velocity(:) = elements(n)%velocity(:) - dx(:)*cortex_constant1
        elements(nn)%velocity(:)= elements(nn)%velocity(:)+ dx(:)*cortex_constant1
      elseif(cells(elements(n)%parent)%fate.EQ.1) then
        elements(n)%velocity(:) = elements(n)%velocity(:) - dx(:)*cortex_constant1*pairs_cortex(m)%cortex_factor
        elements(nn)%velocity(:)= elements(nn)%velocity(:)+ dx(:)*cortex_constant1*pairs_cortex(m)%cortex_factor
      else
        elements(n)%velocity(:) = elements(n)%velocity(:) - dx(:)*cortex_constant2*pairs_cortex(m)%cortex_factor
        elements(nn)%velocity(:)= elements(nn)%velocity(:)+ dx(:)*cortex_constant2*pairs_cortex(m)%cortex_factor
      endif
    end do
    !$omp end do
    !$omp end parallel

  end subroutine scem_cortical_tension_update
end module scem_3_cortical_tension_update
