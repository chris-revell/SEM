module swptst_module

	implicit none
	
	contains
			
		logical function swptst ( n1, n2, n3, n4, x, y, z )

				!*****************************************************************************80
				!
				!! SWPTST decides whether to replace a diagonal arc by the other.
				!
				!  Discussion:
				!
				!    This function decides whether or not to replace a
				!    diagonal arc in a quadrilateral with the other diagonal.
				!    The decision will be to swap (SWPTST = TRUE) if and only
				!    if N4 lies above the plane (in the half-space not contain-
				!    ing the origin) defined by (N1,N2,N3), or equivalently, if
				!    the projection of N4 onto this plane is interior to the
				!    circumcircle of (N1,N2,N3).  The decision will be for no
				!    swap if the quadrilateral is not strictly convex.
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
				!    Input, integer ( kind = 4 ) N1, N2, N3, N4, indexes of the four nodes 
				!    defining the quadrilateral with N1 adjacent to N2, and (N1,N2,N3) in 
				!    counterclockwise order.  The arc connecting N1 to N2 should be replaced 
				!    by an arc connecting N3 to N4 if SWPTST = TRUE.  Refer to subroutine SWAP.
				!
				!    Input, real ( kind = 8 ) X(N), Y(N), Z(N), the coordinates of the nodes. 
				!
				!    Output, logical SWPTST, TRUE if and only if the arc connecting N1
				!    and N2 should be swapped for an arc connecting N3 and N4.
				!
				!  Local parameters:
				!
				!    DX1,DY1,DZ1 = Coordinates of N4->N1
				!    DX2,DY2,DZ2 = Coordinates of N4->N2
				!    DX3,DY3,DZ3 = Coordinates of N4->N3
				!    X4,Y4,Z4 =    Coordinates of N4
				!
	  	  
				  real ( kind = 8 ) dx1
				  real ( kind = 8 ) dx2
				  real ( kind = 8 ) dx3
				  real ( kind = 8 ) dy1
				  real ( kind = 8 ) dy2
				  real ( kind = 8 ) dy3
				  real ( kind = 8 ) dz1
				  real ( kind = 8 ) dz2
				  real ( kind = 8 ) dz3
				  integer ( kind = 4 ) n1
				  integer ( kind = 4 ) n2
				  integer ( kind = 4 ) n3
				  integer ( kind = 4 ) n4
!				  logical              swptst
				  real ( kind = 8 ) x(*)
				  real ( kind = 8 ) x4
				  real ( kind = 8 ) y(*)
				  real ( kind = 8 ) y4
				  real ( kind = 8 ) z(*)
				  real ( kind = 8 ) z4

				  x4 = x(n4)
				  y4 = y(n4)
				  z4 = z(n4)
				  dx1 = x(n1) - x4
				  dx2 = x(n2) - x4
				  dx3 = x(n3) - x4
				  dy1 = y(n1) - y4
				  dy2 = y(n2) - y4
				  dy3 = y(n3) - y4
				  dz1 = z(n1) - z4
				  dz2 = z(n2) - z4
				  dz3 = z(n3) - z4
				!
				!  N4 lies above the plane of (N1,N2,N3) iff N3 lies above
				!  the plane of (N2,N1,N4) iff Det(N3-N4,N2-N4,N1-N4) =
				!  (N3-N4,N2-N4 X N1-N4) > 0.
				!
				  swptst =  dx3 * ( dy2 * dz1 - dy1 * dz2 ) &
						  - dy3 * ( dx2 * dz1 - dx1 * dz2 ) &
						  + dz3 * ( dx2 * dy1 - dx1 * dy2 ) > 0.0D+00
				  
				  return
			end function swptst
end module swptst_module