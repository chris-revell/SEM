! T. J Newman, Tempe, July 2010

module scem_2_initial_create

  use scem_0_arrays
  use scem_0_input
  use scem_0_useful
  use scem_1_types

  implicit none

  contains

    subroutine scem_initial_create

      real*8, dimension(3) :: dx,pos_1,pos_2
      real, dimension(1,3) :: ra
      integer :: flag_success,icount_tot
      integer :: f

      ! initialize position, label, and age of first cell
      do n=1,nc

      	cells(n)%position(:)=(n-1)*r_cell/1.732 ! this is the defined position of the cell - not its center of mass. 1.732 is the square root of 3. Creates 2nd cell 2*r_cell away from the first.
      	cells(n)%label=n
      	cells(n)%age=0.0
      	cells(n)%fate=MOD(n,3)

      	! create elements in random positions in cell volume
      	! exclude insertion of new elements if too close to existing elements
      	icount=0		!counts elements placed for this cell
      	icount_tot=0  !counts element placing attempts for this cell

      	do while (icount.lt.ne_cell) ! counts successful establishment of elements
          flag_success=1 ! initialize success flag
          CALL RANDOM_NUMBER(ra)
          print*, ra
          pos_1(:)=r_cell*(2*ra(1,:)-1) ! define random position of new element w.r.t. origin
          if (dim.eq.2) pos_1(3)=0.0 ! collapse third dimension for planar simulations
          rad_sq=dot_product(pos_1,pos_1)
          if (rad_sq.lt.r_cell_sq) then ! check for successful placement of new element within cell volume
            icount_p=1 ! initialize second counter for existing elements
            do while ((flag_success.eq.1).and.(icount_p.le.icount)) ! loop over existing elements
            	 if (icount.eq.0) then
            	 	pos_2(:)=2*r_cell_sq		!For the very first element, pos_2(:) needs to be given a value because there are no previous elements for it to take values from. Using 2*r_cell_sq ensures that pos_2 is far enough away to not stop the first element from being placed.
               else
               	pos_2(:)=elements(icount_p)%position(:) ! position of existing element icount_p
               endif
               dx(:)=pos_1(:)-pos_2(:) ! relative position of element icount to element icount_p
               dist_sq=dot_product(dx,dx) ! squared distance between the pair of elements
               if (dist_sq.lt.r_close_sq) then ! check that pair of elements are not too close
                  flag_success=0 ! set success flag to failure (element too close to existing element)
               end if
               icount_p=icount_p+1 ! increment counter of existing elements
            end do
          else
            flag_success=0 ! set success flag to failure (element not in sphere of radius r_cell)
          end if
          if (flag_success.eq.1) then ! define properties of successfully placed element
            icount=icount+1 ! increment counter of successfully placed elements
            elements(icount+ne_cell*(n-1))%label=icount ! define new element label
            elements(icount+ne_cell*(n-1))%type=1 ! define type of element
            elements(icount+ne_cell*(n-1))%stage=1 ! define status of element to be steady-state
            elements(icount+ne_cell*(n-1))%strength=1.0 ! weighting for potential interactions
            elements(icount+ne_cell*(n-1))%parent=n ! associate new element with this cell
            elements(icount+ne_cell*(n-1))%position(:)=pos_1(:) ! position of new element w.r.t. origin
            elements(icount+ne_cell*(n-1))%age=0.0 ! initialize age of new element
            elements(icount+ne_cell*(n-1))%velocity(:)=0.0 ! initialize velocity of new element
          end if
          icount_tot=icount_tot+1 ! keep count of total number of attempts to place elements
      	end do
      	write(*,*)"Created cell ", n, " with ", icount, " elements."
	  end do

      ! shift element positions relative to pre-defined position of first cell
      do m=1,ne
      	 f=elements(m)%parent
         elements(m)%position(:)=cells(f)%position(:)+elements(m)%position(:)
      end do

    end subroutine scem_initial_create

end module scem_2_initial_create
