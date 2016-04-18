!C.K. Revell, August 2013

!Converts element positions of a specified cell into spherical polar coordinates
module scem_3_polar

	use scem_0_useful
	use scem_0_arrays
	use scem_0_input
	use scem_1_types

	implicit none

	contains

		subroutine scem_polar(g)

		integer, intent(in) :: g		!Label of cell whose element positions are to be converted to polar coordinates

		double precision, dimension(3) :: r_vector
		double precision :: r_squared
		double precision :: cos_theta
		double precision :: tan_phi

		!Set dimensions of elements_polar to hold positions of each element in the cell. Number of elements given by c_elements(0)
		allocate(elements_polar(cells(g)%c_elements(0), 3))

			do j=1, cells(g)%c_elements(0)
				!Loop over all elements in the cell

				!Calculate polar radius of element
				r_vector(:) = elements(cells(g)%c_elements(j))%position(:) - cells(g)%position(:)
				r_squared = r_vector(1)**2 + r_vector(2)**2 + r_vector(3)**2
				elements_polar(j, 1) = sqrt(r_squared)

				!Calculate polar angle of element
				cos_theta = r_vector(3)/elements_polar(j, 1)
				elements_polar(j,2) = ACOS(cos_theta)

				!Calculate azimuthal angle of the element
				tan_phi = r_vector(2)/r_vector(1)

				if ((r_vector(1).GE.0).AND.(r_vector(2).GE.0)) then
					elements_polar(j,3) = ATAN(tan_phi)
				else
					if ((r_vector(1).LT.0).AND.(r_vector(2).GE.0)) then
						elements_polar(j,3) = pi + ATAN(tan_phi)
					else
						if ((r_vector(1).LT.0).AND.(r_vector(2).LT.0)) then
							elements_polar(j,3) = ATAN(tan_phi) + pi
						else
							if ((r_vector(1).GE.0).AND.(r_vector(2).LT.0)) then
								elements_polar(j,3) = 2*pi + ATAN(tan_phi)
							 end if
					 	end if
					 end if
				end if
			end do

		end subroutine scem_polar

end module scem_3_polar
