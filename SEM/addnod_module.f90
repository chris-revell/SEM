module addnod_module

	use trfind_module
	use intadd_module
	use bdyadd_module
	use covsph_module
	use swap_module
	use lstptr_module
	use swptst_module

	implicit none

	contains

		subroutine addnod ( nst, k, x, y, z, dlist, lptr, lend, lnew, ier )

			!*****************************************************************************80
			!
			!! ADDNOD adds a node to a triangulation.
			!
			!  Discussion:
			!
			!    This subroutine adds node K to a triangulation of the
			!    convex hull of nodes 1, ..., K-1, producing a triangulation
			!    of the convex hull of nodes 1, ..., K.
			!
			!    The algorithm consists of the following steps:  node K
			!    is located relative to the triangulation (TRFIND), its
			!    index is added to the data structure (INTADD or BDYADD),
			!    and a sequence of swaps (SWPTST and SWAP) are applied to
			!    the arcs opposite K so that all arcs incident on node K
			!    and opposite node K are locally optimal (satisfy the circumcircle test).
			!
			!    Thus, if a Delaunay triangulation of nodes 1 through K-1 is input,
			!    a Delaunay triangulation of nodes 1 through K will be output.
			!
			!  Modified:
			!
			!    15 May 2007
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
			!    Input, integer ( kind = 4 ) NST, the index of a node at which TRFIND
			!    begins its search.  Search time depends on the proximity of this node to
			!    K.  If NST < 1, the search is begun at node K-1.
			!
			!    Input, integer ( kind = 4 ) K, the nodal index (index for X, Y, Z, and
			!    LEND) of the new node to be added.  4 <= K.
			!
			!    Input, real ( kind = 8 ) X(K), Y(K), Z(K), the coordinates of the nodes.
			!
			!    Input/output, integer ( kind = 4 ) dlist(6*(N-2)), LPTR(6*(N-2)), LEND(K),
			!    LNEW.  On input, the data structure associated with the triangulation of
			!    nodes 1 to K-1.  On output, the data has been updated to include node
			!    K.  The array lengths are assumed to be large enough to add node K.
			!    Refer to TRMESH.
			!
			!    Output, integer ( kind = 4 ) IER, error indicator:
			!     0 if no errors were encountered.
			!    -1 if K is outside its valid range on input.
			!    -2 if all nodes (including K) are collinear (lie on a common geodesic).
			!     L if nodes L and K coincide for some L < K.
			!
			!  Local parameters:
			!
			!    B1,B2,B3 = Unnormalized barycentric coordinates returned by TRFIND.
			!    I1,I2,I3 = Vertex indexes of a triangle containing K
			!    IN1 =      Vertex opposite K:  first neighbor of IO2
			!               that precedes IO1.  IN1,IO1,IO2 are in
			!               counterclockwise order.
			!    IO1,IO2 =  Adjacent neighbors of K defining an arc to
			!               be tested for a swap
			!    IST =      Index of node at which TRFIND begins its search
			!    KK =       Local copy of K
			!    KM1 =      K-1
			!    L =        Vertex index (I1, I2, or I3) returned in IER
			!               if node K coincides with a vertex
			!    LP =       dlist pointer
			!    LPF =      dlist pointer to the first neighbor of K
			!    LPO1 =     dlist pointer to IO1
			!    LPO1S =    Saved value of LPO1
			!    P =        Cartesian coordinates of node K
			!

			  integer ( kind = 4 ) k

			  real ( kind = 8 ) b1
			  real ( kind = 8 ) b2
			  real ( kind = 8 ) b3
			  integer ( kind = 4 ) i1
			  integer ( kind = 4 ) i2
			  integer ( kind = 4 ) i3
			  integer ( kind = 4 ) ier
			  integer ( kind = 4 ) in1
			  integer ( kind = 4 ) io1
			  integer ( kind = 4 ) io2
			  integer ( kind = 4 ) ist
			  integer ( kind = 4 ) kk
			  integer ( kind = 4 ) km1
			  integer ( kind = 4 ) l
			  integer ( kind = 4 ) lend(k)
			  integer ( kind = 4 ) dlist(*)
			  integer ( kind = 4 ) lnew
			  integer ( kind = 4 ) lp
			  integer ( kind = 4 ) lpf
			  integer ( kind = 4 ) lpo1
			  integer ( kind = 4 ) lpo1s
			  integer ( kind = 4 ) lptr(*)
!			  integer ( kind = 4 ) lstptr
			  integer ( kind = 4 ) nst
			  real ( kind = 8 ) p(3)
!			  logical swptst
			  real ( kind = 8 ) x(k)
			  real ( kind = 8 ) y(k)
			  real ( kind = 8 ) z(k)

			  kk = k

			  if ( kk < 4 ) then
				ier = -1
				write ( *, '(a)' ) ' '
				write ( *, '(a)' ) 'ADDNOD - Fatal error!'
				write ( *, '(a)' ) '  K < 4.'
				stop
			  end if
			!
			!  Initialization:
			!
			  km1 = kk - 1
			  ist = nst
			  if ( ist < 1 ) then
				ist = km1
			  end if

			  p(1) = x(kk)
			  p(2) = y(kk)
			  p(3) = z(kk)
			!
			!  Find a triangle (I1,I2,I3) containing K or the rightmost
			!  (I1) and leftmost (I2) visible boundary nodes as viewed
			!  from node K.
			!
			  call trfind ( ist, p, km1, x, y, z, dlist, lptr, lend, b1, b2, b3, i1, i2, i3 )
			!
			!  Test for collinear or duplicate nodes.
			!
			  if ( i1 == 0 ) then
				ier = -2
				write ( *, '(a)' ) ' '
				write ( *, '(a)' ) 'ADDNOD - Fatal error!'
				write ( *, '(a)' ) '  The nodes are coplanar.'
				stop
			  end if

			  if ( i3 /= 0 ) then

				l = i1

				if ( p(1) == x(l) .and. p(2) == y(l)  .and. p(3) == z(l) ) then
				  ier = l
				  write ( *, '(a)' ) ' '
				  write ( *, '(a)' ) 'ADDNOD - Fatal error!'
				  write ( *, '(a,i8,a,i8)' ) '  Node ', l, ' is equal to node ', k
				  stop
				end if

				l = i2

				if ( p(1) == x(l) .and. p(2) == y(l)  .and. p(3) == z(l) ) then
				  ier = l
				  write ( *, '(a)' ) ' '
				  write ( *, '(a)' ) 'ADDNOD - Fatal error!'
				  write ( *, '(a,i8,a,i8)' ) '  Node ', l, ' is equal to node ', k
				  stop
				end if

				l = i3
				if ( p(1) == x(l) .and. p(2) == y(l)  .and. p(3) == z(l) ) then
				  ier = l
				  write ( *, '(a)' ) ' '
				  write ( *, '(a)' ) 'ADDNOD - Fatal error!'
				  write ( *, '(a,i8,a,i8)' ) '  Node ', l, ' is equal to node ', k
				  stop
				end if

				call intadd ( kk, i1, i2, i3, dlist, lptr, lend, lnew )

			  else

				if ( i1 /= i2 ) then
				  call bdyadd ( kk, i1,i2, dlist, lptr, lend, lnew )
				else
				  call covsph ( kk, i1, dlist, lptr, lend, lnew )
				end if

			  end if

			  ier = 0
			!
			!  Initialize variables for optimization of the triangulation.
			!
			  lp = lend(kk)
			  lpf = lptr(lp)
			  io2 = dlist(lpf)
			  lpo1 = lptr(lpf)
			  io1 = abs ( dlist(lpo1) )
			!
			!  Begin loop: find the node opposite K.
			!
			  do

				lp = lstptr ( lend(io1), io2, dlist, lptr )

				if ( 0 <= dlist(lp) ) then

				  lp = lptr(lp)
				  in1 = abs ( dlist(lp) )
			!
			!  Swap test:  if a swap occurs, two new arcs are
			!  opposite K and must be tested.
			!
				  lpo1s = lpo1

				  if ( .not. swptst ( in1, kk, io1, io2, x, y, z ) ) then

					if ( lpo1 == lpf .or. dlist(lpo1) < 0 ) then
					  exit
					end if

					io2 = io1
					lpo1 = lptr(lpo1)
					io1 = abs ( dlist(lpo1) )
					cycle

				  end if

				  call swap ( in1, kk, io1, io2, dlist, lptr, lend, lpo1 )
			!
			!  A swap is not possible because KK and IN1 are already
			!  adjacent.  This error in SWPTST only occurs in the
			!  neutral case and when there are nearly duplicate nodes.
			!
				  if ( lpo1 /= 0 ) then
					io1 = in1
					cycle
				  end if

				  lpo1 = lpo1s

				end if
			!
			!  No swap occurred.  Test for termination and reset IO2 and IO1.
			!
				if ( lpo1 == lpf .or. dlist(lpo1) < 0 ) then
				  exit
				end if

				io2 = io1
				lpo1 = lptr(lpo1)
				io1 = abs ( dlist(lpo1) )

			  end do

			  return
		end subroutine addnod
end module addnod_module
