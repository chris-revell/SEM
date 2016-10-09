!Module to measure the average velocity of each cell type relative to the centre of mass of that cell type.
!CK Revell october 2016

!module scem_2_measure_velocity

!  use

!  implicit none

!contains

!  subroutine scem_measure_velocity

!    real*8 :: distance_from_COM
!    real*8 ::

!    open(unit=39,file=output_folder//'/system_data/sorting_data_velocity.txt', status='unknown')

    !Find unit vector from type COM to cell. Project cell velocity along unit vector?
    !Still needs cell velocities. Apply to element velocities?
    !Or somehow store set of previous
