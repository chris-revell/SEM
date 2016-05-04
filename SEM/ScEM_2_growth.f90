! T. J Newman, Tempe, September 2010

module scem_2_growth

  use scem_0_arrays
  use scem_0_input
  use scem_0_ran_array
  use scem_0_useful
  use scem_1_types

  implicit none

  contains

    subroutine scem_growth

      real, dimension(1,1) :: ra1
      real, dimension(1,3) :: ra2
      real*8, dimension(3) :: pos,d_pos,pos_1,pos_2,dx
      integer, dimension(3) :: ixe

      real :: rn
      real*8 :: sep_sq
      real*8 :: r_core,phi,theta
      integer :: n_el_cell_k,nn_old,flag_success,flag_placement

      ! attempt creation of new element in each cell
      ! (use an improved algorithm here? - I think we need to eventually - talk to Mikael Bjorkland?)
      do k=1,nc
         call ran_array(ra1,1,1,iseed) ! retrieve one random number
         rn=ra1(1,1)
         if (rn.lt.prob_new_element) then			!prob_new_element comes from ScEM_0_input
            flag_success=0
            n_el_cell_k=cells(k)%c_elements(0)
            r_core=frac_growth*cells(k)%rad_gyration ! defining radius of growth core in terms of cell's rad. of gyr.
            do while (flag_success.eq.0) ! attempt placement of new element until successful
               call ran_array(ra1,1,1,iseed) ! retrieve one random number
               rn=ra1(1,1)
               m=1+int(rn*cells(k)%c_elements(0)) ! randomly select an element in cell k
               n=cells(k)%c_elements(m)
               pos(:)=elements(n)%position(:)-cells(k)%position(:)
               rad_sq=dot_product(pos,pos)
               if (rad_sq.lt.r_core**2) then ! check for successful placement of new element within cell core
                  flag_success=1
                  ne=ne+1
                  call ran_array(ra2,1,2,iseed) ! retrieve two random numbers for random orientation
                  phi=2*pi*ra2(1,1) ! azimuthal angle
                  theta=acos(2*(ra2(1,2)-0.5)) ! polar angle, appropriately weighted
                  d_pos(1)=0.6*r_equil*sin(theta)*cos(phi) ! define random x-position of new element rel. to element n
                  d_pos(2)=0.6*r_equil*sin(theta)*sin(phi) ! define random y-position of new element rel. to element n
                  d_pos(3)=0.6*r_equil*cos(theta) ! define random z-position of new element rel. to element n
                  if (dim.eq.2) d_pos(3)=0.0 ! collapse third dimension for planar simulations
                  elements(ne)%label=ne
                  elements(ne)%parent=k
                  elements(ne)%stage=0
                  elements(ne)%type=1
                  elements(ne)%age=0.0
                  elements(ne)%strength=0.0
                  elements(ne)%position(:)=cells(k)%position(:)+pos(:)+d_pos(:)
                  elements(ne)%velocity(:)=0.0
                  cells(k)%c_elements(0)=n_el_cell_k+1
                  cells(k)%c_elements(n_el_cell_k+1)=ne
               end if
            end do
            ! now some bookkeeping
            ! record position for a possible relisting trigger
            xe_compare(ne,:)=elements(ne)%position(:)
            ! add new pairs (of new element) to pair array
            ! code lifted verbatim from pairs module - code here saves a global relisting and global pair construction
            pos_1(:)=elements(ne)%position(:)
            ! adding 1 in next statement for more meaningful sector assignment
            ixe(:)=1+int((elements(ne)%position(:)+x_cen(:))*recip_sector_size)
            do ix=-1,1
               do iy=-1,1
                  do iz=-1,1
                     nn=head(ixe(1)+ix,ixe(2)+iy,ixe(3)+iz)
                     do while (nn.ne.0)
                        pos_2(:)=elements(nn)%position(:)
                        dx(:)=pos_1(:)-pos_2(:)
                        sep_sq=dot_product(dx,dx)
                        if (sep_sq.le.sector_size_sq) then
                           np=np+1
                           ! note nn < ne always
                           pairs(np,1)=nn
                           pairs(np,2)=ne
                        end if
                        nn=list(nn)
                     end do
                  end do
               end do
            end do
            ! and add new element to list array
            nn=head(ixe(1),ixe(2),ixe(3))
            do while (nn.ne.0)
               nn_old=nn
               nn=list(nn)
            end do
!            list(nn_old)=ne
            list(ne)=0
         end if
      end do

    end subroutine scem_growth

end module scem_2_growth
