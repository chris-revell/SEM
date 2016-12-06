module covsph_module
	
	use insert_module

	implicit none
	
	contains
	
		subroutine covsph ( kk, n0, dlist, lptr, lend, lnew )

			!*****************************************************************************80
			!
			!! COVSPH connects an exterior node to boundary nodes, covering the sphere.
			!
			!  Discussion:
			!
			!    This subroutine connects an exterior node KK to all
			!    boundary nodes of a triangulation of KK-1 points on the
			!    unit sphere, producing a triangulation that covers the
			!    sphere.  The data structure is updated with the addition
			!    of node KK, but no optimization is performed.  All 
			!    boundary nodes must be visible from node KK.
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
			!    Input, integer ( kind = 4 ) KK = Index of the node to be connected to the
			!    set of all boundary nodes.  4 <= KK.
			!
			!    Input, integer ( kind = 4 ) N0 = Index of a boundary node (in the range
			!    1 to KK-1).  N0 may be determined by TRFIND.
			!
			!    Input/output, integer ( kind = 4 ) dlist(6*(N-2)), LPTR(6*(N-2)), LEND(N),
			!    LNEW, the triangulation data structure created by TRMESH.  Node N0 must
			!    be included in the triangulation.  On output, updated with the addition 
			!    of node KK as the last entry.  The updated triangulation contains no
			!    boundary nodes.
			!
			!  Local parameters:
			!
			!    K =     Local copy of KK
			!    LP =    dlist pointer
			!    LSAV =  dlist pointer
			!    NEXT =  Boundary node visible from K
			!    NST =   Local copy of N0
			!

			  integer ( kind = 4 ) k
			  integer ( kind = 4 ) kk
			  integer ( kind = 4 ) lend(*)
			  integer ( kind = 4 ) dlist(*)
			  integer ( kind = 4 ) lnew
			  integer ( kind = 4 ) lp
			  integer ( kind = 4 ) lptr(*)
			  integer ( kind = 4 ) lsav
			  integer ( kind = 4 ) n0
			  integer ( kind = 4 ) next
			  integer ( kind = 4 ) nst

			  k = kk
			  nst = n0
			!
			!  Traverse the boundary in clockwise order, inserting K as
			!  the first neighbor of each boundary node, and converting
			!  the boundary node to an interior node.
			!
			  next = nst

			  do

				lp = lend(next)
				call insert ( k, lp, dlist, lptr, lnew )
				next = -dlist(lp)
				dlist(lp) = next

				if ( next == nst ) then
				  exit
				end if

			  end do
			!
			!  Traverse the boundary again, adding each node to K's adjacency dlist.
			!
			  lsav = lnew

			  do

				lp = lend(next)
				dlist(lnew) = next
				lptr(lnew) = lnew + 1
				lnew = lnew + 1
				next = dlist(lp)

				if ( next == nst ) then
				  exit
				end if

			  end do

			  lptr(lnew-1) = lsav
			  lend(k) = lnew - 1

			  return
		end subroutine covsph
end module covsph_module