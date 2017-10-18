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
			if (randomising) open(unit=45,file=output_folder//'/radius_dist.txt', status='unknown',position="append")
			if (premeanradius.LT.preradius_measurement) radius_pre_below = radius_pre_below+1
			if (epimeanradius.GT.epiradius_measurement) radius_epi_above = radius_epi_above+1
			if (syspremeanradius.LT.syspreradius_measurement) radius_pre_sys_below = radius_pre_sys_below+1
			if (sysepimeanradius.GT.sysepiradius_measurement) radius_epi_sys_above = radius_epi_sys_above+1
			WRITE(45,"(*(G0,:,1X))") time, epimeanradius, sysepimeanradius, premeanradius, syspremeanradius
			if (premeanradius.GT.rad_max) then
				rad_max = premeanradius
				do i=1,nc
					stored_fates_max_rad(i) = cells(i)%fate
				enddo
			endif 
			close(45)
		else
			epiradius_measurement = epimeanradius
			preradius_measurement = premeanradius
		  sysepiradius_measurement = sysepimeanradius
			syspreradius_measurement = syspremeanradius
			open(unit=35,file=output_folder//'/sorting_data/radius.txt', status='unknown',position="append")
			write(35,"(*(G0,:,1X))") time, epimeanradius,sysepimeanradius,premeanradius,syspremeanradius
			close(35)
		endif

	end subroutine scem_measure_radius

end module scem_2_measure_radius
