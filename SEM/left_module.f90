module left_module

	implicit none

	contains

		function left ( x1, y1, z1, x2, y2, z2, x0, y0, z0 )

			!*****************************************************************************80
			!
			!! LEFT determines whether a node is to the left of a plane through the origin.
			!
			!  Discussion:
			!
			!    This function determines whether node N0 is in the
			!    (closed) left hemisphere defined by the plane containing
			!    N1, N2, and the origin, where left is defined relative to
			!    an observer at N1 facing N2.
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
			!    Input, real ( kind = 8 ) X1, Y1, Z1 = Coordinates of N1.
			!
			!    Input, real ( kind = 8 ) X2, Y2, Z2 = Coordinates of N2.
			!
			!    Input, real ( kind = 8 ) X0, Y0, Z0 = Coordinates of N0.
			!
			!    Output, logical LEFT = TRUE if and only if N0 is in the closed
			!    left hemisphere.
			!

			  logical           left
			  real ( kind = 8 ) x0
			  real ( kind = 8 ) x1
			  real ( kind = 8 ) x2
			  real ( kind = 8 ) y0
			  real ( kind = 8 ) y1
			  real ( kind = 8 ) y2
			  real ( kind = 8 ) z0
			  real ( kind = 8 ) z1
			  real ( kind = 8 ) z2
			!
			!  LEFT = TRUE iff <N0,N1 X N2> = det(N0,N1,N2) >= 0.
			!
			  left = x0 * ( y1 * z2 - y2 * z1 ) - y0 * ( x1 * z2 - x2 * z1 ) + z0 * ( x1 * y2 - x2 * y1 ) >= 0.0D+00

			  return
		end function left
end module left_module
