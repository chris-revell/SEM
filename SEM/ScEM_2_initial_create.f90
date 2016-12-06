! T. J Newman, Tempe, July 2010
! Create elements in random positions in cell volume; exclude insertion of new elements if too close to existing elements

module scem_2_initial_create

  use scem_0_arrays
  use scem_0_input
  use scem_1_types

  implicit none

contains

  subroutine scem_initial_create

    integer              :: icount!,j
    !real                 :: dist_sq
    real*8, dimension(3) :: pos_1!,pos_2,dx
    real, dimension(3)   :: ra
    logical              :: success

    intro = .TRUE.

    nc=1
    ne=nc*ne_cell
    np=4*ne
    nc_size=4*nc
    ne_size=4*ne
    np_size=4*np
    ! allocate cell and element data arrays
    allocate(cells(nc_size))
    allocate(elements(ne_size))

    ! initialize position, label, and age of first cell
  	cells(1)%position(:)=0
  	cells(1)%label=1
    cells(1)%age=0.0
    cells(1)%fate=1

    !Add elements to cell:
    icount=0		!counts elements placed for this cell
  	do while (icount.lt.ne_cell)

      ! Define random position of new element. Adjust random numbers to be between plus or minus 1 by subtracting 1 and doubling.
      CALL RANDOM_NUMBER(ra)
      pos_1(:)=r_cell*(ra(:)-0.5)
      success = .TRUE.

      if (DOT_PRODUCT(pos_1,pos_1).GT.0.5*r_cell_sq) success = .FALSE. !Restrict to spherical initial system

      !The following block checks to ensure that new elements are not created too close to existing elements
      !Do not perform this loop if icount=0. If there are no existing elements, this new element will always be placed successfully so long as it is within the spherical radius of the cell
      !if (icount.GT.0.AND.success) then
      !  j=1 ! loop counter
      !  do while (success.and.(j.le.icount)) ! loop j over existing elements
      !    pos_2(:)=elements(j)%position(:)   ! position of existing element j
      !    dx(:)=pos_1(:)-pos_2(:)
      !    dist_sq=dot_product(dx,dx)
      !    if (dist_sq.lt.r_close_sq) then ! check that pair of elements are not too close
      !      success=.FALSE. ! set success flag to failure (element too close to existing element) and return to redefine pos_1
      !    end if
      !    j=j+1 ! increment counter of existing elements
      !  end do
      !endif

      ! If element was successfully places at pos_1, define properties of new element
      if (success) then
        icount=icount+1                       ! increment counter of successfully placed elements
        elements(icount)%label=icount         ! define new element label
        elements(icount)%type=2               ! define type of element
        elements(icount)%stage=1              ! define status of element to be steady-state
        elements(icount)%strength=1.0         ! weighting for potential interactions
        elements(icount)%parent=1             ! associate new element with this cell
        elements(icount)%position(:)=pos_1(:) ! position of new element w.r.t. origin
        elements(icount)%age=0.0              ! initialize age of new element
        elements(icount)%velocity(:)=0.0      ! initialize velocity of new element
      endif

  	end do

    write(*,'(A20,I3,A10)')"Created 1 cell with ", icount, " elements."

  end subroutine scem_initial_create

end module scem_2_initial_create
