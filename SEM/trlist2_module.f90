module trlist2_module

	use trmesh_module

	implicit none

	contains 
	
		subroutine trlist2 ( n, list, lptr, lend, nt, ltri, ier )
	
			!*****************************************************************************80
			!
			!! TRLIST2 converts a triangulation data structure to a triangle list.
			!
			!  Discussion:
			!
			!    This subroutine converts a triangulation data structure
			!    from the linked list created by TRMESH to a triangle list.
			!
			!    It is a version of TRLIST for the special case where the triangle
			!    list should only include the nodes that define each triangle.
			!
			!  Modified:
			!
			!    21 July 2007
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
			!    Input, integer ( kind = 4 ) N, the number of nodes in the triangulation.
			!    3 <= N.
			!
			!    Input, integer ( kind = 4 ) LIST(6*(N-2)), LPTR(6*(N-2)), LEND(N), linked
			!    list data structure defining the triangulation.  Refer to TRMESH.
			!
			!    Output, integer ( kind = 4 ) NT, the number of triangles in the 
			!    triangulation unless IER /=0, in which case NT = 0.  NT = 2N-NB-2 if 
			!    NB >= 3 or 2N-4 if NB = 0, where NB is the number of boundary nodes.
			!
			!    Output, integer ( kind = 4 ) LTRI(3,*).  The second dimension of LTRI
			!    must be at least NT, where NT will be at most 2*N-4.  The J-th column 
			!    contains the vertex nodal indexes associated with triangle J for 
			!    J = 1,...,NT.  The vertices are ordered counterclockwise with the first
			!    vertex taken to be the one with smallest index.  Thus, LTRI(2,J) and 
			!    LTRI(3,J) are larger than LTRI(1,J) and index adjacent neighbors of node 
			!    LTRI(1,J).  The triangles are ordered on first (smallest) vertex indexes.
			!
			!    Output, integer ( kind = 4 ) IER, error indicator.
			!    0, if no errors were encountered.
			!    1, if N is outside its valid range on input.
			!    2, if the triangulation data structure (LIST,LPTR,LEND) is invalid.  
			!      Note, however, that these arrays are not completely tested for validity.
			!
			!  Local parameters:
			!
			!    I,J =      LTRI row indexes (1 to 3) associated with
			!               triangles KT and KN, respectively
			!    I1,I2,I3 = Nodal indexes of triangle KN
			!    ISV =      Variable used to permute indexes I1,I2,I3
			!    KA =       Arc index and number of currently stored arcs
			!    KN =       Index of the triangle that shares arc I1-I2 with KT
			!    KT =       Triangle index and number of currently stored triangles
			!    LP =       LIST pointer
			!    LP2 =      Pointer to N2 as a neighbor of N1
			!    LPL =      Pointer to the last neighbor of I1
			!    LPLN1 =    Pointer to the last neighbor of N1
			!    N1,N2,N3 = Nodal indexes of triangle KT
			!    NM2 =      N-2
			!
	
			  integer ( kind = 4 ) n
	
			  integer ( kind = 4 ) i
			  integer ( kind = 4 ) i1
			  integer ( kind = 4 ) i2
			  integer ( kind = 4 ) i3
			  integer ( kind = 4 ) ier
			  integer ( kind = 4 ) isv
			  integer ( kind = 4 ) j
			  integer ( kind = 4 ) ka
			  integer ( kind = 4 ) kn
			  integer ( kind = 4 ) kt
			  integer ( kind = 4 ) lend(n)
			  integer ( kind = 4 ) list(6*(n-2))
			  integer ( kind = 4 ) lp
			  integer ( kind = 4 ) lp2
			  integer ( kind = 4 ) lpl
			  integer ( kind = 4 ) lpln1
			  integer ( kind = 4 ) lptr(6*(n-2))
			  integer ( kind = 4 ) ltri(3,(2*n-4))
			  integer ( kind = 4 ) n1
			  integer ( kind = 4 ) n2
			  integer ( kind = 4 ) n3
			  integer ( kind = 4 ) nm2
			  integer ( kind = 4 ) nt
			!
			!  Test for invalid input parameters.
			!
			  if ( n < 3 ) then
				nt = 0
				ier = 1
				return
			  end if
			!
			!  Initialize parameters for loop on triangles KT = (N1,N2,
			!  N3), where N1 < N2 and N1 < N3.
			!
			!  KA,KT = Numbers of currently stored arcs and triangles.
			!  NM2 = Upper bound on candidates for N1.
			!
			  ka = 0
			  kt = 0
			  nm2 = n-2
			!
			!  Loop on nodes N1.
			!
			  do n1 = 1, nm2
			!
			!  Loop on pairs of adjacent neighbors (N2,N3).  LPLN1 points
			!  to the last neighbor of N1, and LP2 points to N2.
			!
				lpln1 = lend(n1)
				lp2 = lpln1

			1   continue

				  lp2 = lptr(lp2)
				  n2 = list(lp2)
				  lp = lptr(lp2)
				  n3 = abs ( list(lp) )

				  if ( n2 < n1 .or. n3 < n1 ) then
					go to 8
				  end if
				  
			!
			!  Add a new triangle KT = (N1,N2,N3).
			!
				  kt = kt + 1
				  ltri(1,kt) = n1
				  ltri(2,kt) = n2
				  ltri(3,kt) = n3
			!
			!  Loop on triangle sides (I2,I1) with neighboring triangles
			!  KN = (I1,I2,I3).
			!
				  do i = 1, 3

					if ( i == 1 ) then
					  i1 = n3
					  i2 = n2
					else if ( i == 2 ) then
					  i1 = n1
					  i2 = n3
					else
					  i1 = n2
					  i2 = n1
					end if
			!
			!  Set I3 to the neighbor of I1 that follows I2 unless
			!  I2->I1 is a boundary arc.
			!
					lpl = lend(i1)
					lp = lptr(lpl)

					do

					  if ( list(lp) == i2 ) then
						go to 3
					  end if

					  lp = lptr(lp)

					  if ( lp == lpl ) then
						exit
					  end if

					end do
			!
			!  Invalid triangulation data structure:  I1 is a neighbor of
			!  I2, but I2 is not a neighbor of I1.
			!
					if ( abs ( list(lp) ) /= i2 ) then
					  nt = 0
					  ier = 2
					  return
					end if
			!
			!  I2 is the last neighbor of I1.  Bypass the search for a neighboring
			!  triangle if I2->I1 is a boundary arc.
			!
					kn = 0

					if ( list(lp) < 0 ) then
					  go to 6
					end if
			!
			!  I2->I1 is not a boundary arc, and LP points to I2 as
			!  a neighbor of I1.
			!
			3       continue

					lp = lptr(lp)
					i3 = abs ( list(lp) )
			!
			!  Find J such that LTRI(J,KN) = I3 (not used if KT < KN),
			!  and permute the vertex indexes of KN so that I1 is smallest.
			!
					if ( i1 < i2 .and. i1 < i3 ) then
					  j = 3
					else if ( i2 < i3 ) then
					  j = 2
					  isv = i1
					  i1 = i2
					  i2 = i3
					  i3 = isv
					else
					  j = 1
					  isv = i1
					  i1 = i3
					  i3 = i2
					  i2 = isv
					end if
			!
			!  Test for KT < KN (triangle index not yet assigned).
			!
					if ( n1 < i1 ) then
					  cycle
					end if
			!
			!  Find KN, if it exists, by searching the triangle list in
			!  reverse order.
			!
					do kn = kt-1, 1, -1
					  if ( ltri(1,kn) == i1 .and. &
						   ltri(2,kn) == i2 .and. &
						   ltri(3,kn) == i3 ) then
						go to 5
					  end if
					end do

					cycle

			5       continue

			6       continue

					end do
			!
			!  Bottom of loop on triangles.
			!
			8     continue

				  if ( lp2 /= lpln1 ) then
					go to 1
				  end if

			9     continue

			  end do

			  nt = kt
			  ier = 0

			  return
		end subroutine trlist2
end module trlist2_module