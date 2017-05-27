!Module to calculate the radius of each preblast cell from the system centre of mass, normliased by the maximum radius of any cell in the system.
!C.K.Revell, November 2013, Updated October 2016

module scem_2_measure_radius

	use scem_0_input
	use scem_0_arrays
	use scem_1_types

	implicit none

contains

	subroutine scem_measure_radius

		integer :: i,precount,epicount
		real*8	:: premeanradius,epimeanradius,sysmeanradius
		real*8, dimension(3) :: pre_COM,epi_COM,sys_COM		!Vector position of system centres of mass
		real*8, dimension(3) :: cell_vector		!Vector position of cell relative to system centre of mass

		if (.NOT.randomising) open(unit=35,file=output_folder//'/sorting_data/radius.txt', status='unknown',position="append")

		!Need to start by calculating the centre of mass of the system, which can change after each iteration due to cell movement.
		pre_COM(:)= 0
		epi_COM(:)= 0
		sys_COM(:)= 0
		precount  = 0
		epicount  = 0
		do i=1, ne
			sys_COM(:)=sys_COM(:)+elements(i)%position(:)
			if (cells(elements(i)%parent)%fate.EQ.2) then
				pre_COM(:)=pre_COM(:)+elements(i)%position(:)
				precount = precount + 1
			else
				epi_COM(:)=epi_COM(:)+elements(i)%position(:)
				epicount = epicount + 1
			endif
		enddo
		pre_COM(:)=pre_COM(:)/precount  !Centre of mass found by dividing sum by total mass, ie total number of elements.
		epi_COM(:)=epi_COM(:)/epicount
		sys_COM(:)=sys_COM(:)/ne

		!Now switching from elements to cells
		precount = 0
		premeanradius = 0
		epicount = 0
		epimeanradius = 0
		sysmeanradius = 0
		do i=1, nc
			if (cells(i)%fate.EQ.2) then
				cell_vector = cells(i)%position - pre_COM
				premeanradius = premeanradius + SQRT(DOT_PRODUCT(cell_vector,cell_vector))*cells(i)%c_elements(0)
				precount = precount + 1
			else
				cell_vector = cells(i)%position - epi_COM
				epimeanradius = epimeanradius + SQRT(DOT_PRODUCT(cell_vector,cell_vector))*cells(i)%c_elements(0)
				epicount = epicount + 1

				cell_vector = cells(i)%position - sys_COM
				sysmeanradius = sysmeanradius + SQRT(DOT_PRODUCT(cell_vector,cell_vector))*cells(i)%c_elements(0)
			endif
		enddo
		premeanradius = premeanradius/precount
		epimeanradius = epimeanradius/epicount
		sysmeanradius = sysmeanradius/epicount

		if (randomising) then
			radius1_mean = radius1_mean + epimeanradius
			radius1_min  = MIN(radius1_min,epimeanradius)
			radius2_mean = radius2_mean + premeanradius
			radius2_max  = MAX(radius2_max,premeanradius)
			radius3_mean = radius3_mean + sysmeanradius
			radius3_min  = MIN(radius3_min,sysmeanradius)
		else
			write(35,*) time, epimeanradius, premeanradius, sysmeanradius
		endif

		close(35)

	end subroutine scem_measure_radius

end module scem_2_measure_radius
