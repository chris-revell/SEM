! T. J Newman, Tempe, February 2011

module scem_2_resize

  use scem_0_arrays
  use scem_0_input
  use scem_0_useful
  use scem_1_types

  implicit none

  contains

    ! resize arrays if necessary
    subroutine scem_resize

      type(element), allocatable, dimension(:) :: elements_temp
      type(cell), allocatable, dimension(:) :: cells_temp
      real*8, allocatable, dimension(:,:) :: xe_compare_temp
      real*8, allocatable, dimension(:,:) :: xe_prev_temp
      integer, allocatable, dimension(:) :: list_temp
      integer, allocatable, dimension(:,:) :: pairs_temp
      real*8 :: x_max,y_max,z_max

	  !When the number of elements in the system exceeds half of the dimension size of the elements array (ne_size) then arrays are reallocated
      if (ne.gt.trigger_frac*ne_size) then
         allocate(elements_temp(ne_size))
         allocate(xe_prev_temp(ne_size,3))
         allocate(xe_compare_temp(ne_size,3))
         allocate(list_temp(ne_size))
         elements_temp=elements
         xe_prev_temp=xe_prev
         xe_compare_temp=xe_compare
         list_temp=list
         deallocate(elements)
         deallocate(xe_prev)
         deallocate(xe_compare)
         deallocate(list)
         allocate(elements(2*ne_size))
         allocate(xe_prev(2*ne_size,3))
         allocate(xe_compare(2*ne_size,3))
         allocate(list(2*ne_size))
         elements(1:ne_size)=elements_temp(:)
         xe_prev(1:ne_size,:)=xe_prev_temp(:,:)
         xe_compare(1:ne_size,:)=xe_compare_temp(:,:)
         list(1:ne_size)=list_temp(:)
         !elements(ne_size+1,2*ne_size)=0
         xe_prev(ne_size+1:2*ne_size,:)=0.d0
         xe_compare(ne_size+1:2*ne_size,:)=0.d0
         list(ne_size+1:2*ne_size)=0
         deallocate(elements_temp)
         deallocate(xe_prev_temp)
         deallocate(xe_compare_temp)
         deallocate(list_temp)
         ne_size=2*ne_size
      end if

      if (nc.gt.trigger_frac*nc_size) then
         allocate(cells_temp(nc_size))
         cells_temp=cells
         deallocate(cells)
         allocate(cells(2*nc_size))
         cells(1:nc_size)=cells_temp(:)
         !cells(nc_size+1,2*nc_size)=0
         deallocate(cells_temp)
         nc_size=2*nc_size
      end if

      ! this next block of code is redundant at present as resizing of pairs array is performed in scem_2_pairs
!      if (np.gt.trigger_frac*np_size) then
!         allocate(pairs_temp(np_size,2))
!         pairs_temp=pairs
!         deallocate(pairs)
!         allocate(pairs(2*np_size,2))
!         pairs(1:np_size,:)=pairs_temp(:,:)
!         pairs(np_size+1:2*np_size,:)=0
!         deallocate(pairs_temp)
!         np_size=2*np_size
!      end if

      x_max=0.0
      y_max=0.0
      z_max=0.0
      do k=1,ne
!      	write(*,*) "x_max ", x_max, " element position ", recip_sector_size*elements(k)%position(1)
!         x_max=recip_sector_size*MAX(x_max,ABS(elements(k)%position(1)))
		 x_max=MAX(x_max,(recip_sector_size*(ABS(elements(k)%position(1)))))
!         write(*,*) "new x_max ", x_max
!         write(*,*) "y_max ", y_max, " element position ", recip_sector_size*elements(k)%position(2)
!         y_max=recip_sector_size*MAX(y_max,ABS(elements(k)%position(2)))
		 y_max=MAX(y_max,(recip_sector_size*(ABS(elements(k)%position(2)))))
!         write(*,*) "new y_max ", y_max
!         write(*,*) "z_max ", z_max, " element position ", recip_sector_size*elements(k)%position(3)
!         z_max=recip_sector_size*MAX(z_max,ABS(elements(k)%position(3)))
		 z_max=MAX(z_max,(recip_sector_size*(ABS(elements(k)%position(3)))))
!         write(*,*) "new z_max ", z_max
      end do
!      write(*,*) x_max, y_max, z_max, nx
      if ((x_max.gt.trigger_frac*0.5*nx).or.(y_max.gt.trigger_frac*0.5*ny).or.(z_max.gt.trigger_frac*0.5*nz)) then
         deallocate(head)
         nx=2*nx
         ny=2*ny
         nz=2*nz
         x_cen(1)=sector_size*int(nx/2)
         x_cen(2)=sector_size*int(ny/2)
         x_cen(3)=sector_size*int(nz/2)
         allocate(head(nx,ny,nz))
         flag_relist=1
      end if

    end subroutine scem_resize

end module scem_2_resize
