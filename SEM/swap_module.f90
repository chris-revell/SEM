module swap_module
	
	use lstptr_module

	implicit none
	
	contains

		subroutine swap ( in1, in2, io1, io2, dlist, lptr, lend, lp21 )

			!*****************************************************************************80
			!
			!! SWAP replaces the diagonal arc of a quadrilateral with the other diagonal.
			!
			!  Discussion:
			!
			!    Given a triangulation of a set of points on the unit
			!    sphere, this subroutine replaces a diagonal arc in a
			!    strictly convex quadrilateral (defined by a pair of adja-
			!    cent triangles) with the other diagonal.  Equivalently, a
			!    pair of adjacent triangles is replaced by another pair
			!    having the same union.
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
			!    Input, integer ( kind = 4 ) IN1, IN2, IO1, IO2, nodal indexes of the 
			!    vertices of the quadrilateral.  IO1-IO2 is replaced by IN1-IN2.  
			!    (IO1,IO2,IN1) and (IO2,IO1,IN2) must be triangles on input.
			!
			!    Input/output, integer ( kind = 4 ) dlist(6*(N-2)), LPTR(6*(N-2)), LEND(N),
			!    the data structure defining the triangulation, created by TRMESH.  
			!    On output, updated with the swap; triangles (IO1,IO2,IN1) an (IO2,IO1,IN2) 
			!    are replaced by (IN1,IN2,IO2) and (IN2,IN1,IO1) unless LP21 = 0.
			!
			!    Output, integer ( kind = 4 ) LP21, index of IN1 as a neighbor of IN2 after
			!    the swap is performed unless IN1 and IN2 are adjacent on input, in which 
			!    case LP21 = 0.
			!
			!  Local parameters:
			!
			!    LP, LPH, LPSAV = dlist pointers
			!

			  integer ( kind = 4 ) in1
			  integer ( kind = 4 ) in2
			  integer ( kind = 4 ) io1
			  integer ( kind = 4 ) io2
			  integer ( kind = 4 ) lend(*)
			  integer ( kind = 4 ) dlist(*)
			  integer ( kind = 4 ) lp
			  integer ( kind = 4 ) lp21
			  integer ( kind = 4 ) lph
			  integer ( kind = 4 ) lpsav
			  integer ( kind = 4 ) lptr(*)
!			  integer ( kind = 4 ) lstptr
			!
			!  Test for IN1 and IN2 adjacent.
			!
			  lp = lstptr ( lend(in1), in2, dlist, lptr )

			  if ( abs ( dlist(lp) ) == in2 ) then
				lp21 = 0
				return
			  end if
			!
			!  Delete IO2 as a neighbor of IO1.
			!
			  lp = lstptr ( lend(io1), in2, dlist, lptr )
			  lph = lptr(lp)
			  lptr(lp) = lptr(lph)
			!
			!  If IO2 is the last neighbor of IO1, make IN2 the last neighbor.
			!
			  if ( lend(io1) == lph ) then
				lend(io1) = lp
			  end if
			!
			!  Insert IN2 as a neighbor of IN1 following IO1 using the hole created above.
			!
			  lp = lstptr ( lend(in1), io1, dlist, lptr )
			  lpsav = lptr(lp)
			  lptr(lp) = lph
			  dlist(lph) = in2
			  lptr(lph) = lpsav
			!
			!  Delete IO1 as a neighbor of IO2.
			!
			  lp = lstptr ( lend(io2), in1, dlist, lptr )
			  lph = lptr(lp)
			  lptr(lp) = lptr(lph)
			!
			!  If IO1 is the last neighbor of IO2, make IN1 the last neighbor.
			!
			  if ( lend(io2) == lph ) then
				lend(io2) = lp
			  end if
			!
			!  Insert IN1 as a neighbor of IN2 following IO2.
			!
			  lp = lstptr ( lend(in2), io2, dlist, lptr )
			  lpsav = lptr(lp)
			  lptr(lp) = lph
			  dlist(lph) = in1
			  lptr(lph) = lpsav
			  lp21 = lph

			  return
		end subroutine swap
end module swap_module