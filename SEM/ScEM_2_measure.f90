!C.K.Revell, November 2013

module scem_2_measure

	use scem_0_input
	use scem_0_arrays
	use scem_0_useful
	use scem_1_types

	implicit none

	contains

		subroutine scem_measure

		integer :: sorting_count

		open(unit=35,file=output_folder//'/system_data/sorting_data.txt', status='unknown')

		sorting_count=0

		do m=1, np																								!Loop over all interaction pairs
			if (elements(pairs(m,1))%parent.EQ.elements(pairs(m,2))%parent) then
				!Do nothing for element pairs within the came cell
			else
				if (cells(elements(pairs(m,1))%parent)%fate.EQ.cells(elements(pairs(m,2))%parent)%fate) then	!If both elements in an interaction pair belong to cells of the same type
					sorting_count=sorting_count+1																!then increment counter by one. Final value of this counter after each timestep
				end if																							!gives a quantifiable measure of sorting
			end if
		end do

		write(35,*) real(time), real(sorting_count)/real(np)													!Print counter to data file for each timestep, normalised by the number of pairs
		call flush(35)
		end subroutine scem_measure

end module scem_2_measure
