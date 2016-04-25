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

		double precision, dimension(3) :: r_vector
		double precision :: r_squared
		double precision :: cos_theta
		double precision :: tan_phi

		!Loop over all elements in the system
		do i=1, ne

			!Calculate polar radius of element
			r_vector(:) = elements(i)%position(:) - cells(elements(i)%parent)%position(:)
			r_squared = r_vector(1)**2 + r_vector(2)**2 + r_vector(3)**2
			elements(i)%polar(1) = sqrt(r_squared)

			!Calculate polar angle of element
			cos_theta = r_vector(3)/elements(i)%polar(1)
			elements(i)%polar(2) = ACOS(cos_theta)

			!Calculate azimuthal angle of the element
			tan_phi = r_vector(1)/r_vector(2)

			if ((r_vector(1).GE.0).AND.(r_vector(2).GE.0)) then
				elements(i)%polar(3) = ATAN(tan_phi)
				print*, elements(i)%polar(3), ATAN2(r_vector(2),r_vector(1)), elements(i)%polar(3)-pi+ATAN2(r_vector(2),r_vector(1))
			elseif ((r_vector(1).LT.0).AND.(r_vector(2).GE.0)) then
				elements(i)%polar(3) = pi + ATAN(tan_phi)
				print*, elements(i)%polar(3), ATAN2(r_vector(2),r_vector(1)), elements(i)%polar(3)-pi+ATAN2(r_vector(2),r_vector(1))
			elseif ((r_vector(1).LT.0).AND.(r_vector(2).LT.0)) then
				elements(i)%polar(3) = ATAN(tan_phi) + pi
				print*, elements(i)%polar(3), ATAN2(r_vector(2),r_vector(1)), elements(i)%polar(3)-pi+ATAN2(r_vector(2),r_vector(1))
			else
				!((r_vector(1).GE.0).AND.(r_vector(2).LT.0))
				elements(i)%polar(3) = 2*pi + ATAN(tan_phi)
				print*, elements(i)%polar(3), ATAN2(r_vector(2),r_vector(1)), elements(i)%polar(3)-pi+ATAN2(r_vector(2),r_vector(1))
			end if

		end do

	end subroutine scem_polar

end module scem_2_polar
