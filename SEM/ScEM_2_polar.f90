!C.K. Revell, August 2013

!Converts element positions of a specified cell into spherical polar coordinates
module scem_2_polar

	use scem_0_useful
	use scem_0_arrays
	use scem_0_input
	use scem_1_types

	implicit none

	contains

		subroutine scem_polar

		real*8, dimension(3) :: r_vector
		real*8							 :: r_squared
		real*8							 :: cos_theta

		!Loop over all elements in the system
		do i=1, ne

			!Calculate polar radius of element
			r_vector(:) = elements(i)%position(:) - cells(elements(i)%parent)%position(:)
			r_squared = r_vector(1)**2 + r_vector(2)**2 + r_vector(3)**2
			elements(i)%polar(1) = sqrt(r_squared)

			!Calculate polar angle of element
			cos_theta = r_vector(3)/elements(i)%polar(1)
			elements(i)%polar(2) = ACOS(cos_theta)

			!Calculate azimuthal angle of the element. Add pi to define angle between 0 and 2pi rather than -pi and +pi.
			elements(i)%polar(3) = pi+ATAN2(r_vector(2),r_vector(1))

		end do

	end subroutine scem_polar

end module scem_2_polar
