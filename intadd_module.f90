module intadd_module
		
	use insert_module
	use lstptr_module
	
	implicit none
		
	contains

		subroutine intadd ( kk, i1, i2, i3, list, lptr, lend, lnew )

			!*****************************************************************************80
			!
			!! INTADD adds an interior node to a triangulation.
			!
			!  Discussion:
			!
			!    This subroutine adds an interior node to a triangulation
			!    of a set of points on the unit sphere.  The data structure
			!    is updated with the insertion of node KK into the triangle
			!    whose vertices are I1, I2, and I3.  No optimization of the
			!    triangulation is performed.
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
			!    Input, integer ( kind = 4 ) KK, the index of the node to be inserted. 
			!    1 <= KK and KK must not be equal to I1, I2, or I3.
			!
			!    Input, integer ( kind = 4 ) I1, I2, I3, indexes of the 
			!    counterclockwise-ordered sequence of vertices of a triangle which contains 
			!    node KK.
			!
			!    Input, integer ( kind = 4 ) LIST(6*(N-2)), LPTR(6*(N-2)), LEND(N), LNEW, 
			!    the data structure defining the triangulation, created by TRMESH.  Triangle
			!    (I1,I2,I3) must be included in the triangulation.
			!    On output, updated with the addition of node KK.  KK
			!    will be connected to nodes I1, I2, and I3.
			!
			!  Local parameters:
			!
			!    K =        Local copy of KK
			!    LP =       LIST pointer
			!    N1,N2,N3 = Local copies of I1, I2, and I3
			!
			  implicit none

			  integer ( kind = 4 ) i1
			  integer ( kind = 4 ) i2
			  integer ( kind = 4 ) i3
			  integer ( kind = 4 ) k
			  integer ( kind = 4 ) kk
			  integer ( kind = 4 ) lend(*)
			  integer ( kind = 4 ) list(*)
			  integer ( kind = 4 ) lnew
			  integer ( kind = 4 ) lp
			  integer ( kind = 4 ) lptr(*)
!			  integer ( kind = 4 ) lstptr
			  integer ( kind = 4 ) n1
			  integer ( kind = 4 ) n2
			  integer ( kind = 4 ) n3

			  k = kk
			!
			!  Initialization.
			!
			  n1 = i1
			  n2 = i2
			  n3 = i3
			!
			!  Add K as a neighbor of I1, I2, and I3.
			!
			  lp = lstptr ( lend(n1), n2, list, lptr )
			  call insert ( k, lp, list, lptr, lnew )

			  lp = lstptr ( lend(n2), n3, list, lptr )
			  call insert ( k, lp, list, lptr, lnew )

			  lp = lstptr ( lend(n3), n1, list, lptr )
			  call insert ( k, lp, list, lptr, lnew )
			!
			!  Add I1, I2, and I3 as neighbors of K.
			!
			  list(lnew) = n1
			  list(lnew+1) = n2
			  list(lnew+2) = n3
			  lptr(lnew) = lnew + 1
			  lptr(lnew+1) = lnew + 2
			  lptr(lnew+2) = lnew
			  lend(k) = lnew + 2
			  lnew = lnew + 3

			  return
		end subroutine intadd
end module intadd_module