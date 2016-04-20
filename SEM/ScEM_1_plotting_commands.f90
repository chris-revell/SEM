module scem_1_plotting_commands

  use scem_0_input
  use scem_0_arrays
  use scem_0_useful

  implicit none

contains

  subroutine scem_plotting_commands

  open(unit=45,file=output_folder//'/system_data/gnuplot_commands_system_plots.gnu',status='unknown')

  write(45,"(*(G0,:,1X))") 'set nokey'
  write(45,"(*(G0,:,1X))") 'set terminal png'
  write(45,"(*(G0,:,1X))") 'set title "Neighbour sorting measurement against time"'
  write(45,"(*(G0,:,1X))") 'set output "'//output_folder//'/system_data/sorting_neighbours.png"'
  write(45,"(*(G0,:,1X))") 'set xlabel "Time /s"'
  write(45,"(*(G0,:,1X))") 'set ylabel "Ratio of same fate neighbour pairs \nto total number of neighbour pairs"'
  write(45,"(*(G0,:,1X))") 'set yrange [0:1]'
  write(45,"(*(G0,:,1X))") 'plot "'//output_folder//'/system_data/sorting_data_neighbours.txt" with lines'
  write(45,"(*(G0,:,1X))")

  write(45,"(*(G0,:,1X))") 'set nokey'
  write(45,"(*(G0,:,1X))") 'unset yrange'
  write(45,"(*(G0,:,1X))") 'set terminal png'
  write(45,"(*(G0,:,1X))") 'set title "Radius sorting measurement against time"'
  write(45,"(*(G0,:,1X))") 'set output "'//output_folder//'/system_data/sorting_radius.png"'
  write(45,"(*(G0,:,1X))") 'set xlabel "Time /s"'
  write(45,'(A51)',advance='no') 'set ylabel "Difference between average radius of \n'
  write(45,"(*(G0,:,1X))") 'cell types relative to system centre of mass"'
  write(45,"(*(G0,:,1X))") 'plot "'//output_folder//'/system_data/sorting_data_radius.txt" with lines'
  write(45,"(*(G0,:,1X))")

  write(45,"(*(G0,:,1X))") 'set nokey'
  write(45,"(*(G0,:,1X))") 'set terminal png'
  write(45,"(*(G0,:,1X))") 'set title "Element pair sorting measurement against time"'
  write(45,"(*(G0,:,1X))") 'set output "'//output_folder//'/system_data/sorting_element_pairs.png"'
  write(45,"(*(G0,:,1X))") 'set xlabel "Time /s"'
  write(45,'(A47)',advance='no') 'set ylabel "Ratio of same fate element pairs \n'
  write(45,"(*(G0,:,1X))") 'to total number of element pairs"'
  write(45,"(*(G0,:,1X))") 'set yrange [0:1]'
  write(45,"(*(G0,:,1X))") 'plot "'//output_folder//'/system_data/sorting_data.txt" with lines'
  write(45,"(*(G0,:,1X))")

  write(45,"(*(G0,:,1X))") 'set term png'
  write(45,"(*(G0,:,1X))") 'set nokey'
  write(45,"(*(G0,:,1X))") 'unset yrange'
  write(45,"(*(G0,:,1X))") 'set title "Number of cells in system against time"'
  write(45,"(*(G0,:,1X))") 'set output "'//output_folder//'/system_data/cell_count.png"'
  write(45,"(*(G0,:,1X))") 'set xlabel "Time /s"'
  write(45,"(*(G0,:,1X))") 'set ylabel "Number of cells in system"'
  write(45,"(*(G0,:,1X))") 'plot "'//output_folder//'/system_data/cell_count.txt" with lines'
  write(45,"(*(G0,:,1X))")

  write(45,"(*(G0,:,1X))") 'set title "Cell volumes against time"'
  write(45,"(*(G0,:,1X))") 'set key'
  write(45,"(*(G0,:,1X))") 'set xlabel "Time /s"'
  write(45,"(*(G0,:,1X))") 'set ylabel "Volume /?"'
  write(45,"(*(G0,:,1X))") 'set output "'//output_folder//'/system_data/cell_volumes.png"'
  write(45,'(A69)',advance="no") ' plot "'//output_folder//'/system_data/cell_volumes.txt" using 1:2 '
  write(45,'(A26)',advance="no") 'title "cell  1" with lines'
  if (nc.GT.1) then
    do n=2, nc
      if (n.LT.9) then
        write(45,'(A63)',advance="no") ', "'//output_folder//'/system_data/cell_volumes.txt" using 1:'
        write(45,'(I1,A13)',advance="no") n+1, ' title "cell '
        if (n.LT.nc) then
          write(45,'(I1,A13)') n, '" with lines\'
        else
          write(45,'(I1,A12)') n, '" with lines'
        endif
      else
        write(45,'(A63)',advance="no") ', "'//output_folder//'/system_data/cell_volumes.txt" using 1:'
        write(45,'(I2,A13)',advance="no") n+1, ' title "cell '
        if (n.LT.nc) then
          write(45,'(I2,A13)') n, '" with lines\'
        else
          write(45,'(I2,A12)') n, '" with lines'
        endif
      endif
    enddo
  endif

  end subroutine scem_plotting_commands

end module scem_1_plotting_commands
