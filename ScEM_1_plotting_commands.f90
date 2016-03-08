module scem_1_plotting_commands

  use scem_0_input
  use scem_0_arrays
  use scem_0_useful

  implicit none

contains

  subroutine scem_plotting_commands

  open(unit=45,file='data/system_data/gnuplot_commands_system_plots.gnu',status='unknown')

  write(45,*) 'set nokey'
  write(45,*) 'set terminal png'
  write(45,*) 'set title "Neighbour sorting measurement against time"'
  write(45,*) 'set output "data/system_data/sorting_neighbours.png"'
  write(45,*) 'set xlabel "Time /s"'
  write(45,*) 'set ylabel "Ratio of same fate neighbour pairs \nto total number of neighbour pairs"'
  write(45,*) 'set yrange [0:1]'
  write(45,*) 'plot "data/system_data/sorting_data_neighbours.txt" with lines'
  write(45,*)

  write(45,*) 'set nokey'
  write(45,*) 'unset yrange'
  write(45,*) 'set terminal png'
  write(45,*) 'set title "Radius sorting measurement against time"'
  write(45,*) 'set output "data/system_data/sorting_radius.png"'
  write(45,*) 'set xlabel "Time /s"'
  write(45,'(A52)',advance='no') ' set ylabel "Difference between average radius of \n'
  write(45,*) 'cell types relative to system centre of mass"'
  write(45,*) 'plot "data/system_data/sorting_data_radius.txt" with lines'
  write(45,*)

  write(45,*) 'set nokey'
  write(45,*) 'set terminal png'
  write(45,*) 'set title "Element pair sorting measurement against time"'
  write(45,*) 'set output "data/system_data/sorting_element_pairs.png"'
  write(45,*) 'set xlabel "Time /s"'
  write(45,'(A48)',advance='no') ' set ylabel "Ratio of same fate element pairs \n'
  write(45,*) 'to total number of element pairs"'
  write(45,*) 'set yrange [0:1]'
  write(45,*) 'plot "data/system_data/sorting_data.txt" with lines'
  write(45,*)

  write(45,*) 'set term png'
  write(45,*) 'set nokey'
  write(45,*) 'unset yrange'
  write(45,*) 'set title "Number of cells in system against time"'
  write(45,*) 'set output "data/system_data/cell_count.png"'
  write(45,*) 'set xlabel "Time /s"'
  write(45,*) 'set ylabel "Number of cells in system"'
  write(45,*) 'plot "data/system_data/cell_count.txt" with lines'
  write(45,*)

  write(45,*) 'set title "Cell volumes against time"'
  write(45,*) 'set key'
  write(45,*) 'set xlabel "Time /s"'
  write(45,*) 'set ylabel "Volume /?"'
  write(45,*) 'set output "data/system_data/cell_volumes.png"'
  write(45,'(A52)',advance="no") ' plot "data/system_data/cell_volumes.txt" using 1:2 '
  write(45,'(A26)',advance="no") 'title "cell  1" with lines'
  if (nc.GT.1) then
    do n=2, nc
      if (n.LT.9) then
        write(45,'(A46)',advance="no") ', "data/system_data/cell_volumes.txt" using 1:'
        write(45,'(I1,A13)',advance="no") n+1, ' title "cell '
        if (n.LT.nc) then
          write(45,'(I1,A13)') n, '" with lines\'
        else
          write(45,'(I1,A12)') n, '" with lines'
        endif
      else
        write(45,'(A46)',advance="no") ', "data/system_data/cell_volumes.txt" using 1:'
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
