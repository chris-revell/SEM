module trmesh_module

	use left_module
	use addnod_module

	implicit none

	contains

		subroutine trmesh ( n, x, y, z, dlist, lptr, lend, lnew, near, next, dist, ier )

			!*****************************************************************************
			!
			!! TRMESH creates a Delaunay triangulation on the unit sphere.
			!
			!  Discussion:
			!
			!    This subroutine creates a Delaunay triangulation of a
			!    set of N arbitrarily distributed points, referred to as
			!    nodes, on the surface of the unit sphere.  The Delaunay
			!    triangulation is defined as a set of (spherical) triangles
			!    with the following five properties:
			!
			!     1)  The triangle vertices are nodes.
			!     2)  No triangle contains a node other than its vertices.
			!     3)  The interiors of the triangles are pairwise disjoint.
			!     4)  The union of triangles is the convex hull of the set
			!           of nodes (the smallest convex set that contains
			!           the nodes).  If the nodes are not contained in a
			!           single hemisphere, their convex hull is the
			!           entire sphere and there are no boundary nodes.
			!           Otherwise, there are at least three boundary nodes.
			!     5)  The interior of the circumcircle of each triangle
			!           contains no node.
			!
			!    The first four properties define a triangulation, and the
			!    last property results in a triangulation which is as close
			!    as possible to equiangular in a certain sense and which is
			!    uniquely defined unless four or more nodes lie in a common
			!    plane.  This property makes the triangulation well-suited
			!    for solving closest-point problems and for triangle-based
			!    interpolation.
			!
			!    Provided the nodes are randomly ordered, the algorithm
			!    has expected time complexity O(N*log(N)) for most nodal
			!    distributions.  Note, however, that the complexity may be
			!    as high as O(N**2) if, for example, the nodes are ordered
			!    on increasing latitude.
			!
			!    Spherical coordinates (latitude and longitude) may be
			!    converted to Cartesian coordinates by Subroutine TRANS.
			!
			!    The following is a dlist of the software package modules
			!    which a user may wish to call directly:
			!
			!    ADDNOD - Updates the triangulation by appending a new node.
			!
			!    AREAS  - Returns the area of a spherical triangle.
			!
			!    BNODES - Returns an array containing the indexes of the
			!             boundary nodes (if any) in counterclockwise
			!             order.  Counts of boundary nodes, triangles,
			!             and arcs are also returned.
			!
			!    CIRCUM - Returns the circumcenter of a spherical triangle.
			!
			!    CRLIST - Returns the set of triangle circumcenters
			!             (Voronoi vertices) and circumradii associated
			!             with a triangulation.
			!
			!    DELARC - Deletes a boundary arc from a triangulation.
			!
			!    DELNOD - Updates the triangulation with a nodal deletion.
			!
			!    EDGE   - Forces an arbitrary pair of nodes to be connected
			!             by an arc in the triangulation.
			!
			!    GETNP  - Determines the ordered sequence of L closest nodes
			!             to a given node, along with the associated distances.
			!
			!    INSIDE - Locates a point relative to a polygon on the
			!             surface of the sphere.
			!
			!    INTRSC - Returns the point of intersection between a
			!             pair of great circle arcs.
			!
			!    JRAND  - Generates a uniformly distributed pseudo-random integer.
			!
			!    LEFT   - Locates a point relative to a great circle.
			!
			!    NEARND - Returns the index of the nearest node to an
			!             arbitrary point, along with its squared
			!             distance.
			!
			!    SCOORD - Converts a point from Cartesian coordinates to
			!             spherical coordinates.
			!
			!    STORE  - Forces a value to be stored in main memory so
			!             that the precision of floating point numbers
			!             in memory locations rather than registers is
			!             computed.
			!
			!    TRANS  - Transforms spherical coordinates into Cartesian
			!             coordinates on the unit sphere for input to
			!             Subroutine TRMESH.
			!
			!    TRLIST - Converts the triangulation data structure to a
			!             triangle dlist more suitable for use in a finite
			!             element code.
			!
			!    TRLPRT - Prints the triangle dlist created by TRLIST.
			!
			!    TRMESH - Creates a Delaunay triangulation of a set of
			!             nodes.
			!
			!    TRPLOT - Creates a level-2 Encapsulated Postscript (EPS)
			!             file containing a triangulation plot.
			!
			!    TRPRNT - Prints the triangulation data structure and,
			!             optionally, the nodal coordinates.
			!
			!    VRPLOT - Creates a level-2 Encapsulated Postscript (EPS)
			!             file containing a Voronoi diagram plot.
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
			!    Input, integer ( kind = 4 ) N, the number of nodes in the triangulation.
			!    3 <= N.
			!
			!    Input, real ( kind = 8 ) X(N), Y(N), Z(N), the coordinates of distinct
			!    nodes.  (X(K),Y(K), Z(K)) is referred to as node K, and K is referred
			!    to as a nodal index.  It is required that X(K)**2 + Y(K)**2 + Z(K)**2 = 1
			!    for all K.  The first three nodes must not be collinear (lie on a
			!    common great circle).
			!
			!    Output, integer ( kind = 4 ) dlist(6*(N-2)), nodal indexes which, along
			!    with LPTR, LEND, and LNEW, define the triangulation as a set of N
			!    adjacency lists; counterclockwise-ordered sequences of neighboring nodes
			!    such that the first and last neighbors of a boundary node are boundary
			!    nodes (the first neighbor of an interior node is arbitrary).  In order to
			!    distinguish between interior and boundary nodes, the last neighbor of
			!    each boundary node is represented by the negative of its index.
			!
			!    Output, integer ( kind = 4 ) LPTR(6*(N-2)), = Set of pointers (dlist
			!    indexes) in one-to-one correspondence with the elements of dlist.
			!    dlist(LPTR(I)) indexes the node which follows dlist(I) in cyclical
			!    counterclockwise order (the first neighbor follows the last neighbor).
			!
			!    Output, integer ( kind = 4 ) LEND(N), pointers to adjacency lists.
			!    LEND(K) points to the last neighbor of node K.  dlist(LEND(K)) < 0 if and
			!    only if K is a boundary node.
			!
			!    Output, integer ( kind = 4 ) LNEW, pointer to the first empty location
			!    in dlist and LPTR (dlist length plus one).  dlist, LPTR, LEND, and LNEW are
			!    not altered if IER < 0, and are incomplete if 0 < IER.
			!
			!    Workspace, integer ( kind = 4 ) NEAR(N),
			!    used to efficiently determine the nearest triangulation node to each
			!    unprocessed node for use by ADDNOD.
			!
			!    Workspace, integer ( kind = 4 ) NEXT(N),
			!    used to efficiently determine the nearest triangulation node to each
			!    unprocessed node for use by ADDNOD.
			!
			!    Workspace, real ( kind = 8 ) DIST(N),
			!    used to efficiently determine the nearest triangulation node to each
			!    unprocessed node for use by ADDNOD.
			!
			!    Output, integer ( kind = 4 ) IER, error indicator:
			!     0, if no errors were encountered.
			!    -1, if N < 3 on input.
			!    -2, if the first three nodes are collinear.
			!     L, if nodes L and M coincide for some L < M.  The data structure
			!      represents a triangulation of nodes 1 to M-1 in this case.
			!
			!  Local parameters:
			!
			!    D =        (Negative cosine of) distance from node K to node I
			!    D1,D2,D3 = Distances from node K to nodes 1, 2, and 3, respectively
			!    I,J =      Nodal indexes
			!    I0 =       Index of the node preceding I in a sequence of
			!               unprocessed nodes:  I = NEXT(I0)
			!    K =        Index of node to be added and DO-loop index: 3 < K
			!    LP =       dlist index (pointer) of a neighbor of K
			!    LPL =      Pointer to the last neighbor of K
			!    NEXTI =    NEXT(I)
			!    NN =       Local copy of N
			!

			  integer ( kind = 4 ) n

			  real ( kind = 8 ) d
			  real ( kind = 8 ) d1
			  real ( kind = 8 ) d2
			  real ( kind = 8 ) d3
			  real ( kind = 8 ) dist(n)
			  integer ( kind = 4 ) i
			  integer ( kind = 4 ) i0
			  integer ( kind = 4 ) ier
			  integer ( kind = 4 ) j
			  integer ( kind = 4 ) k
!			  logical              left
			  integer ( kind = 4 ) lend(n)
			  integer ( kind = 4 ) dlist(6*(n-2))
			  integer ( kind = 4 ) lnew
			  integer ( kind = 4 ) lp
			  integer ( kind = 4 ) lpl
			  integer ( kind = 4 ) lptr(6*(n-2))
			  integer ( kind = 4 ) near(n)
			  integer ( kind = 4 ) next(n)
			  integer ( kind = 4 ) nexti
			  integer ( kind = 4 ) nn
			  real ( kind = 8 ) x(n)
			  real ( kind = 8 ) y(n)
			  real ( kind = 8 ) z(n)

			  nn = n


			  if ( nn < 3 ) then
				ier = -1
				write ( *, '(a)' ) ' '
				write ( *, '(a)' ) 'TRMESH - Fatal error!'
				write ( *, '(a)' ) '  N < 3.'
				stop
			  end if

			!
			!  Store the first triangle in the linked dlist.
			!
			  if ( .not. left (x(1),y(1),z(1),x(2),y(2),z(2), &
							   x(3),y(3),z(3) ) ) then
			!
			!  The first triangle is (3,2,1) = (2,1,3) = (1,3,2).
			!
				dlist(1) = 3
				lptr(1) = 2
				dlist(2) = -2
				lptr(2) = 1
				lend(1) = 2

				dlist(3) = 1
				lptr(3) = 4
				dlist(4) = -3
				lptr(4) = 3
				lend(2) = 4

				dlist(5) = 2
				lptr(5) = 6
				dlist(6) = -1
				lptr(6) = 5
				lend(3) = 6

			  else if ( .not. left ( x(2),y(2),z(2),x(1),y(1),z(1),x(3),y(3),z(3) ) ) then
			!
			!  The first triangle is (1,2,3):  3 Strictly Left 1->2,
			!  i.e., node 3 lies in the left hemisphere defined by arc 1->2.
			!
				dlist(1) = 2
				lptr(1) = 2
				dlist(2) = -3
				lptr(2) = 1
				lend(1) = 2

				dlist(3) = 3
				lptr(3) = 4
				dlist(4) = -1
				lptr(4) = 3
				lend(2) = 4

				dlist(5) = 1
				lptr(5) = 6
				dlist(6) = -2
				lptr(6) = 5
				lend(3) = 6
			!
			!  The first three nodes are collinear.
			!
			  else

				ier = -2
				write ( *, '(a)' ) ' '
				write ( *, '(a)' ) 'TRMESH - Fatal error!'
				write ( *, '(a)' ) '  The first 3 nodes are collinear.'
				write ( *, '(a)' ) '  Try reordering the data.'
				stop

			  end if
			!
			!  Initialize LNEW and test for N = 3.
			!
			  lnew = 7

			  if ( nn == 3 ) then
				ier = 0
				return
			  end if
			!
			!  A nearest-node data structure (NEAR, NEXT, and DIST) is
			!  used to obtain an expected-time (N*log(N)) incremental
			!  algorithm by enabling constant search time for locating
			!  each new node in the triangulation.
			!
			!  For each unprocessed node K, NEAR(K) is the index of the
			!  triangulation node closest to K (used as the starting
			!  point for the search in Subroutine TRFIND) and DIST(K)
			!  is an increasing function of the arc length (angular
			!  distance) between nodes K and NEAR(K):  -Cos(a) for arc
			!  length a.
			!
			!  Since it is necessary to efficiently find the subset of
			!  unprocessed nodes associated with each triangulation
			!  node J (those that have J as their NEAR entries), the
			!  subsets are stored in NEAR and NEXT as follows:  for
			!  each node J in the triangulation, I = NEAR(J) is the
			!  first unprocessed node in J's set (with I = 0 if the
			!  set is empty), L = NEXT(I) (if 0 < I) is the second,
			!  NEXT(L) (if 0 < L) is the third, etc.  The nodes in each
			!  set are initially ordered by increasing indexes (which
			!  maximizes efficiency) but that ordering is not main-
			!  tained as the data structure is updated.
			!
			!  Initialize the data structure for the single triangle.
			!
			  near(1) = 0
			  near(2) = 0
			  near(3) = 0

			  do k=nn, 4, -1

				d1 = -( x(k) * x(1) + y(k) * y(1) + z(k) * z(1) )
				d2 = -( x(k) * x(2) + y(k) * y(2) + z(k) * z(2) )
				d3 = -( x(k) * x(3) + y(k) * y(3) + z(k) * z(3) )

				if ( d1 <= d2 .and. d1 <= d3 ) then
				  near(k) = 1
				  dist(k) = d1
				  next(k) = near(1)
				  near(1) = k
				else if ( d2 <= d1 .and. d2 <= d3 ) then
				  near(k) = 2
				  dist(k) = d2
				  next(k) = near(2)
				  near(2) = k
				else
				  near(k) = 3
				  dist(k) = d3
				  next(k) = near(3)
				  near(3) = k
				end if

			  end do
			!
			!  Add the remaining nodes.
			!
			  do k = 4, nn

				call addnod ( near(k), k, x, y, z, dlist, lptr, lend, lnew, ier )

				if ( ier /= 0 ) then
				  write ( *, '(a)' ) ' '
				  write ( *, '(a)' ) 'TRMESH - Fatal error!'
				  write ( *, '(a,i8)' ) '  ADDNOD returned error code IER = ', ier
				  stop
				end if
			!
			!  Remove K from the set of unprocessed nodes associated with NEAR(K).
			!
				i = near(k)

				if ( near(i) == k ) then

				  near(i) = next(k)

				else

				  i = near(i)

				  do

					i0 = i
					i = next(i0)

					if ( i == k ) then
					  exit
					end if

				  end do

				  next(i0) = next(k)

				end if
				near(k) = 0
			!
			!  Loop on neighbors J of node K.
			!
				lpl = lend(k)
				lp = lpl

			3   continue

				lp = lptr(lp)
				j = abs ( dlist(lp) )
			!
			!  Loop on elements I in the sequence of unprocessed nodes
			!  associated with J:  K is a candidate for replacing J
			!  as the nearest triangulation node to I.  The next value
			!  of I in the sequence, NEXT(I), must be saved before I
			!  is moved because it is altered by adding I to K's set.
			!
				i = near(j)
				do

				  if ( i == 0 ) then
					exit
				  end if

				  nexti = next(i)
			!
			!  Test for the distance from I to K less than the distance
			!  from I to J.
			!
				  d = - ( x(i) * x(k) + y(i) * y(k) + z(i) * z(k) )
				  if ( d < dist(i) ) then
			!
			!  Replace J by K as the nearest triangulation node to I:
			!  update NEAR(I) and DIST(I), and remove I from J's set
			!  of unprocessed nodes and add it to K's set.
			!
					near(i) = k
					dist(i) = d

					if ( i == near(j) ) then
					  near(j) = nexti
					else
					  next(i0) = nexti
					end if

					next(i) = near(k)
					near(k) = i
				  else
					i0 = i
				  end if

				  i = nexti

				end do
			!
			!  Bottom of loop on neighbors J.
			!
			5   continue

				if ( lp /= lpl ) then
				  go to 3
				end if
			6   continue
			  end do
			  return
		end subroutine trmesh
end module trmesh_module
