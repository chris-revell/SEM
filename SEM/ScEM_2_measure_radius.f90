!Module to calculate the radius of each preblast cell from the system centre of mass, normliased by the maximum radius of any cell in the system.
!C.K.Revell, November 2013, Updated October 2016

module scem_2_measure_radius

	use scem_0_input
	use scem_0_arrays
	use scem_1_types

	implicit none

contains

	subroutine scem_measure_radius

		integer :: i
		real*8	:: syspremeanradius,sysepimeanradius,premeanradius,epimeanradius
		real*8, dimension(3) :: cell_vector		!Vector position of cell relative to system centre of mass

		epimeanradius = 0
		premeanradius = 0
		sysepimeanradius = 0
		syspremeanradius = 0
		do i=1, nc
			if (cells(i)%fate.EQ.2) then
				cell_vector = cells(i)%position - pre_com
				premeanradius = premeanradius + SQRT(DOT_PRODUCT(cell_vector,cell_vector))!*cells(i)%c_elements(0)
				cell_vector = cells(i)%position - sys_com
				syspremeanradius = syspremeanradius + SQRT(DOT_PRODUCT(cell_vector,cell_vector))!*cells(i)%c_elements(0)
			else
				cell_vector = cells(i)%position - epi_com
				epimeanradius = epimeanradius + SQRT(DOT_PRODUCT(cell_vector,cell_vector))!*cells(i)%c_elements(0)
				cell_vector = cells(i)%position - sys_com
				sysepimeanradius = sysepimeanradius + SQRT(DOT_PRODUCT(cell_vector,cell_vector))!*cells(i)%c_elements(0)
			endif
		enddo
		premeanradius = premeanradius/(nc-epicellcount)!(ne-epielementcount)
		epimeanradius = epimeanradius/epicellcount!epielementcount
		syspremeanradius = syspremeanradius/(nc-epicellcount)!(ne-epielementcount)
		sysepimeanradius = sysepimeanradius/epicellcount!epielementcount

		if (randomising) then
			random_values_radius(ran_loop,:) = (/epimeanradius, sysepimeanradius, premeanradius, syspremeanradius/)
		else
			open(unit=35,file=output_folder//'/sorting_data/radius.txt', status='unknown',position="append")
			write(35,"(*(G0,:,1X))") time, epimeanradius,sysepimeanradius,premeanradius,syspremeanradius
			close(35)
		endif

	end subroutine scem_measure_radius

end module scem_2_measure_radius
