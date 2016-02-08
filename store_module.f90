module store_module

	implicit none
	
	contains

		real function store ( x )

				!*****************************************************************************80
				!
				!! STORE forces its argument to be stored.
				!
				!  Discussion:
				!
				!    This function forces its argument X to be stored in a
				!    memory location, thus providing a means of determining
				!    floating point number characteristics (such as the machine
				!    precision) when it is necessary to avoid computation in
				!    high precision registers.
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
				!    Input, real ( kind = 8 ) X, the value to be stored.
				!
				!    Output, real ( kind = 8 ) STORE, the value of X after it has been stored
				!    and possibly truncated or rounded to the single precision word length.
				!

!				  real ( kind = 8 ) store
				  real ( kind = 8 ) x
				  real ( kind = 8 ) y

				  common /stcom/ y

				  y = x
				  store = y

			end function store
end module store_module