module lstptr_module

	implicit none
	
	contains			
			
			integer function lstptr ( lpl, nb, list, lptr )

				!*****************************************************************************80
				!
				!! LSTPTR returns the index of NB in the adjacency list.
				!
				!  Discussion:
				!
				!    This function returns the index (LIST pointer) of NB in
				!    the adjacency list for N0, where LPL = LEND(N0).
				!
				!    This function is identical to the similarly named function in TRIPACK.
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
				!    Input, integer ( kind = 4 ) LPL, is LEND(N0).
				!
				!    Input, integer ( kind = 4 ) NB, index of the node whose pointer is to 
				!    be returned.  NB must be connected to N0.
				!
				!    Input, integer ( kind = 4 ) LIST(6*(N-2)), LPTR(6*(N-2)), the data 
				!    structure defining the triangulation, created by TRMESH.
				!
				!    Output, integer ( kind = 4 ) LSTPTR, pointer such that LIST(LSTPTR) = NB or
				!    LIST(LSTPTR) = -NB, unless NB is not a neighbor of N0, in which 
				!    case LSTPTR = LPL.
				!
				!  Local parameters:
				!
				!    LP = LIST pointer
				!    ND = Nodal index
				!

				  integer ( kind = 4 ) list(*)
				  integer ( kind = 4 ) lp
				  integer ( kind = 4 ) lpl
				  integer ( kind = 4 ) lptr(*)
!				  integer ( kind = 4 ) lstptr
				  integer ( kind = 4 ) nb
				  integer ( kind = 4 ) nd

				  lp = lptr(lpl)

				  do

					nd = list(lp)

					if ( nd == nb ) then
					  exit
					end if

					lp = lptr(lp)

					if ( lp == lpl ) then
					  exit
					end if

				  end do

				  lstptr = lp
				  
				  return
			end function lstptr
end module lstptr_module