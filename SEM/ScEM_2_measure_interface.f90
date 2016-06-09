!C.K.Revell, November 2013

module scem_2_measure_interface

	use scem_0_arrays
	use scem_0_useful
	use scem_1_types

	implicit none

contains

	subroutine scem_measure_interface

		integer :: like_like_count
		integer :: inter_cell_count
		integer :: cell1
		integer :: cell2
		real    :: interface_sorting_measurement

		open(unit=35,file=output_folder//'/system_data/sorting_data_interface.txt', status='unknown')

		like_like_count  = 0
		inter_cell_count = 0

		do m=1, np																	!Loop over all interaction pairs
			cell1 = elements(pairs(m,1))%parent					!Label of parent cell of the 1st element in the pair
			cell2 = elements(pairs(m,2))%parent					!Label of parent cell of the 2nd element in the pair

			if (cell1.EQ.cell2) then
				!Intra-cell pair, so does not contribute to interface measurements.
				CYCLE
			elseif (cells(cell1)%fate.EQ.cells(cell2)%fate) then
				!If both elements in an interaction pair are in different parent cells but those cells are
				!of the same type, then increment like-like counter by one.
				!Also increment the total interface counter
				like_like_count	 = like_like_count +1
				inter_cell_count = inter_cell_count+1
			else
				!Pair acts between elements in two different parent cells,
				!but those parent cells do not have the same type.
				!Increment total interface counter but not the like-like counter
				inter_cell_count = inter_cell_count+1
			end if
		end do

		!Interface sorting measurement is the ratio of the like-like counter to the total inter-cell counter.
		!ie the ratio of like-like interface area to total inter-cell interface area.
		if (inter_cell_count.EQ.0) then
			interface_sorting_measurement = 0
		else
			interface_sorting_measurement = real(like_like_count)/real(inter_cell_count)
		endif

		write(35,*) real(time), interface_sorting_measurement			!Print counter to data file for each timestep, normalised by the number of pairs

	end subroutine scem_measure_interface

end module scem_2_measure_interface
