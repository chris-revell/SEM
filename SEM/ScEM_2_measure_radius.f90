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

!		if (.NOT.randomising) open(unit=35,file=output_folder//'/sorting_data/radius.txt', status='unknown',position="append")

		epimeanradius = 0
		premeanradius = 0
		sysepimeanradius = 0
		syspremeanradius = 0
		do i=1, nc
			if (cells(i)%fate.EQ.2) then
				cell_vector = cells(i)%position - pre_com
				premeanradius = premeanradius + SQRT(DOT_PRODUCT(cell_vector,cell_vector))!*cells(i)%c_elements(0)
				cell_vector = cells(i)%position - sys_com
				syspremeanradius = syspremeanradius + SQRT(DOT_PRODUCT(cell_vector,cell_vector))
			else
				cell_vector = cells(i)%position - epi_com
				epimeanradius = epimeanradius + SQRT(DOT_PRODUCT(cell_vector,cell_vector))!*cells(i)%c_elements(0)
				cell_vector = cells(i)%position - sys_com
				sysepimeanradius = sysepimeanradius + SQRT(DOT_PRODUCT(cell_vector,cell_vector))
			endif
		enddo
		premeanradius = premeanradius/(nc-epicellcount)
		epimeanradius = epimeanradius/epicellcount
		syspremeanradius = syspremeanradius/(nc-epicellcount)
		sysepimeanradius = sysepimeanradius/epicellcount

		if (randomising) then
			if (premeanradius.LE.preradius_measurement) radius_pre_below = radius_pre_below+1
			if (epimeanradius.LE.epiradius_measurement) radius_epi_below = radius_epi_below+1
			if (syspremeanradius.LE.syspreradius_measurement) radius_pre_sys_below = radius_pre_sys_below+1
			if (sysepimeanradius.LE.sysepiradius_measurement) radius_epi_sys_below = radius_epi_sys_below+1
		else
			epiradius_measurement = epimeanradius
			preradius_measurement = premeanradius
		  sysepiradius_measurement = sysepimeanradius
			syspreradius_measurement = syspremeanradius
		endif

	!	close(35)

	end subroutine scem_measure_radius

end module scem_2_measure_radius
