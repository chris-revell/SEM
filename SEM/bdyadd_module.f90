module bdyadd_module
		
	use insert_module
	
	implicit none

	contains

		subroutine bdyadd ( kk, i1, i2, dlist, lptr, lend, lnew )

			!*****************************************************************************80
			!
			!! BDYADD adds a boundary node to a triangulation.
			!
			!  Discussion:
			!
			!    This subroutine adds a boundary node to a triangulation
			!    of a set of KK-1 points on the unit sphere.  The data
			!    structure is updated with the insertion of node KK, but no
			!    optimization is performed.
			!
			!    This routine is identical to the similarly named routine
			!    in TRIPACK.
			!
			!  Modified:
			!
			!    16 June 2007
			!
			!  Author:
			!
			!    Robert Renka
			!
			!  Reference:
			!
			!    Robert Renka,
			!    Algorithm 772: STRIPACK,
			!    Delaunay Triangulation and Voronoi Diagram on the Surface of a Sphere,
			!    ACM Transactions on Mathematical Software,
			!    Volume 23, Number 3, September 1997, pages 416-434.
			!
			!  Parameters:
			!
			!    Input, integer ( kind = 4 ) KK, the index of a node to be connected to 
			!    the sequence of all visible boundary nodes.  1 <= KK and
			!    KK must not be equal to I1 or I2.
			!
			!    Input, integer ( kind = 4 ) I1, the first (rightmost as viewed from KK) 
			!    boundary node in the triangulation that is visible from
			!    node KK (the line segment KK-I1 intersects no arcs.
			!
			!    Input, integer ( kind = 4 ) I2, the last (leftmost) boundary node that 
			!    is visible from node KK.  I1 and I2 may be determined by TRFIND.
			!
			!    Input/output, integer ( kind = 4 ) dlist(6*(N-2)), LPTR(6*(N-2)), LEND(N),
			!    LNEW, the triangulation data structure created by TRMESH.  
			!    Nodes I1 and I2 must be included 
			!    in the triangulation.  On output, the data structure is updated with
			!    the addition of node KK.  Node KK is connected to I1, I2, and
			!    all boundary nodes in between.
			!
			!  Local parameters:
			!
			!    K =     Local copy of KK
			!    LP =    dlist pointer
			!    LSAV =  dlist pointer
			!    N1,N2 = Local copies of I1 and I2, respectively
			!    NEXT =  Boundary node visible from K
			!    NSAV =  Boundary node visible from K
			!

			  integer ( kind = 4 ) i1
			  integer ( kind = 4 ) i2
			  integer ( kind = 4 ) k
			  integer ( kind = 4 ) kk
			  integer ( kind = 4 ) lend(*)
			  integer ( kind = 4 ) dlist(*)
			  integer ( kind = 4 ) lnew
			  integer ( kind = 4 ) lp
			  integer ( kind = 4 ) lptr(*)
			  integer ( kind = 4 ) lsav
			  integer ( kind = 4 ) n1
			  integer ( kind = 4 ) n2
			  integer ( kind = 4 ) next
			  integer ( kind = 4 ) nsav

			  k = kk
			  n1 = i1
			  n2 = i2
			!
			!  Add K as the last neighbor of N1.
			!
			  lp = lend(n1)
			  lsav = lptr(lp)
			  lptr(lp) = lnew
			  dlist(lnew) = -k
			  lptr(lnew) = lsav
			  lend(n1) = lnew
			  lnew = lnew + 1
			  next = -dlist(lp)
			  dlist(lp) = next
			  nsav = next
			!
			!  Loop on the remaining boundary nodes between N1 and N2,
			!  adding K as the first neighbor.
			!
			  do

				lp = lend(next)
				call insert ( k, lp, dlist, lptr, lnew )

				if ( next == n2 ) then
				  exit
				end if

				next = -dlist(lp)
				dlist(lp) = next

			  end do
			!
			!  Add the boundary nodes between N1 and N2 as neighbors of node K.
			!
			  lsav = lnew
			  dlist(lnew) = n1
			  lptr(lnew) = lnew + 1
			  lnew = lnew + 1
			  next = nsav

			  do

				if ( next == n2 ) then
				  exit
				end if

				dlist(lnew) = next
				lptr(lnew) = lnew + 1
				lnew = lnew + 1
				lp = lend(next)
				next = dlist(lp)

			  end do

			  dlist(lnew) = -n2
			  lptr(lnew) = lsav
			  lend(k) = lnew
			  lnew = lnew + 1

			  return
		end subroutine bdyadd
end module bdyadd_module