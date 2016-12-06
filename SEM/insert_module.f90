module insert_module

	implicit none
	
	contains

		subroutine insert ( k, lp, dlist, lptr, lnew )

			!*****************************************************************************80
			!
			!! INSERT inserts K as a neighbor of N1.
			!
			!  Discussion:
			!
			!    This subroutine inserts K as a neighbor of N1 following
			!    N2, where LP is the dlist pointer of N2 as a neighbor of
			!    N1.  Note that, if N2 is the last neighbor of N1, K will
			!    become the first neighbor (even if N1 is a boundary node).
			!
			!    This routine is identical to the similarly named routine in TRIPACK.
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
			!    Input, integer ( kind = 4 ) K, the index of the node to be inserted.
			!
			!    Input, integer ( kind = 4 ) LP, the dlist pointer of N2 as a neighbor of N1.
			!
			!    Input/output, integer ( kind = 4 ) dlist(6*(N-2)), LPTR(6*(N-2)), LNEW, 
			!    the data structure defining the triangulation, created by TRMESH.
			!    On output, updated with the addition of node K.
			!

			  integer ( kind = 4 ) k
			  integer ( kind = 4 ) dlist(*)
			  integer ( kind = 4 ) lnew
			  integer ( kind = 4 ) lp
			  integer ( kind = 4 ) lptr(*)
			  integer ( kind = 4 ) lsav

			  lsav = lptr(lp)
			  lptr(lp) = lnew
			  dlist(lnew) = k
			  lptr(lnew) = lsav
			  lnew = lnew + 1

			  return
		end subroutine insert
end module insert_module