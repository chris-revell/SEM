module jrand_module

	contains

		integer function jrand ( n, ix, iy, iz )

				!*****************************************************************************80
				!
				!! JRAND returns a random integer between 1 and N.
				!
				!  Discussion:
				!
				!   This function returns a uniformly distributed pseudorandom integer
				!   in the range 1 to N.
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
				!    Brian Wichmann, David Hill,
				!    An Efficient and Portable Pseudo-random Number Generator,
				!    Applied Statistics,
				!    Volume 31, Number 2, 1982, pages 188-190.
				!
				!  Parameters:
				!
				!    Input, integer ( kind = 4 ) N, the maximum value to be returned.
				!
				!    Input/output, integer ( kind = 4 ) IX, IY, IZ = seeds initialized to
				!    values in the range 1 to 30,000 before the first call to JRAND, and
				!    not altered between subsequent calls (unless a sequence of random
				!    numbers is to be repeated by reinitializing the seeds).
				!
				!    Output, integer ( kind = 4 ) JRAND, a random integer in the range 1 to N.
				!
				!  Local parameters:
				!
				!    U = Pseudo-random number uniformly distributed in the interval (0,1).
				!    X = Pseudo-random number in the range 0 to 3 whose fractional part is U.
				!

				  integer ( kind = 4 ) ix
				  integer ( kind = 4 ) iy
				  integer ( kind = 4 ) iz
!				  integer ( kind = 4 ) jrand
				  integer ( kind = 4 ) n
				  real ( kind = 8 ) u
				  real ( kind = 8 ) x

				  ix = mod ( 171 * ix, 30269 )
				  iy = mod ( 172 * iy, 30307 )
				  iz = mod ( 170 * iz, 30323 )

				  x = ( real ( ix, kind = 8 ) / 30269.0D+00 ) + ( real ( iy, kind = 8 ) / 30307.0D+00 ) + ( real ( iz, kind = 8 ) / 30323.0D+00 )

				  u = x - int ( x )
				  jrand = real ( n, kind = 8 ) * u + 1.0D+00

				  return
			end function jrand
end module jrand_module
