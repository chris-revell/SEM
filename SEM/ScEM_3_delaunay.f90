!CK Revell, November 2016

module scem_3_delaunay

  use scem_0_input
  use scem_0_arrays
  use scem_1_types
  use trmesh_module
  use trlist2_module

  implicit none

contains

  subroutine scem_delaunay

    integer                               :: i,j,k
    real*8, allocatable, dimension(:)			:: x					!Position data for cortex elements relative to COM and normalised to unit sphere
    real*8, allocatable, dimension(:)			:: y					!Position data for cortex elements relative to COM and normalised to unit sphere
    real*8, allocatable, dimension(:)			:: z					!Position data for cortex elements relative to COM and normalised to unit sphere
    real*8, dimension(3)                  :: r_vector
    real*8																:: radius_squared		!Square of distance from cell COM to a given element
    real*8																:: radius				!Distance from cell COM to a given element (for normalising to unit sphere)
    integer, allocatable, dimension(:)		:: list
    integer, allocatable, dimension(:)		:: lptr
    integer, allocatable, dimension(:)		:: lend
    integer																:: lnew
    integer, allocatable, dimension(:)		:: near
    integer, allocatable, dimension(:)		:: next
    real*8, allocatable, dimension(:)			:: dist
    integer																:: ier
    integer																:: nt					!Number of neighbour triplets in triangulation
    integer, allocatable, dimension(:,:)	:: ltri					!Array of delaunay triplets. 2nd index is label of triplet, first index is 1-3. So ltri(1,n), ltri(2,n), ltri(3,n) are labels of elements in neighbour triplet n, with ltri(1,n) being the smallest element label.

    np_cortex = 0

    do j=1, nc

      !Allocate element position arrays for delaunay triangulation
      allocate(x(cells(j)%cortex_elements(0)))
      allocate(y(cells(j)%cortex_elements(0)))
      allocate(z(cells(j)%cortex_elements(0)))

      !Fill x,y,z arrays with positions of cortex elements relative to cell centre of mass and projected onto unit sphere.
      do i=1, cells(j)%cortex_elements(0)
        k = cells(j)%cortex_elements(i)
        r_vector = elements(k)%position-cells(j)%position
        radius_squared	= DOT_PRODUCT(r_vector,r_vector)
        radius					= sqrt(radius_squared)
        x(i)						= r_vector(1)/radius
        y(i)						= r_vector(2)/radius
        z(i)						= r_vector(3)/radius
      end do

      !Allocate arrays for trmesh using system values
      allocate(list(6*(cells(j)%cortex_elements(0)-2)))
      allocate(lptr(6*(cells(j)%cortex_elements(0)-2)))
      allocate(lend(6*(cells(j)%cortex_elements(0)-2)))
      allocate(near(cells(j)%cortex_elements(0)))
      allocate(next(cells(j)%cortex_elements(0)))
      allocate(dist(cells(j)%cortex_elements(0)))
      allocate(ltri(3,(2*cells(j)%cortex_elements(0)-4)))

      !Call trmesh to perform triangulation
      call trmesh ( cells(j)%cortex_elements(0), x, y, z, list, lptr, lend, lnew, near, next, dist, ier )

      !Call trlist2 to extract usable triplet list ltri from trmesh results
      call trlist2 ( cells(j)%cortex_elements(0), list, lptr, lend, nt, ltri, ier )

      !Now that we have the list of Delaunay triangles, we can also allocate and fill the triplets array in the
      !cell data structure so that the triplets can be used in other modules.
      !Use cells(j)%cortex_elements() array to convert the label of elements in ltri(:,:) into the global element
      !label in cells(j)%triplets(:,:). This will make life much easier when the triplets array is used later on.
      if (allocated(cells(j)%triplets)) deallocate(cells(j)%triplets)
      allocate(cells(j)%triplets(3,(2*cells(j)%cortex_elements(0)-4)))
      do i=1, nt
        cells(j)%triplets(1,i)=cells(j)%cortex_elements(ltri(1,i))
        cells(j)%triplets(2,i)=cells(j)%cortex_elements(ltri(2,i))
        cells(j)%triplets(3,i)=cells(j)%cortex_elements(ltri(3,i))
      end do
      cells(j)%triplet_count = nt

      np_cortex = np_cortex + 3*nt

      deallocate(x)
      deallocate(y)
      deallocate(z)
      deallocate(list)
      deallocate(lptr)
      deallocate(lend)
      deallocate(near)
      deallocate(next)
      deallocate(dist)
      deallocate(ltri)

    end do

  end subroutine scem_delaunay

end module
