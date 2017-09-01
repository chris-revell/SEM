! this module defines the two major data structures: 'element' and 'cell'

! T. J Newman, Tempe, July 2010

module scem_1_types

  use scem_0_input

  implicit none

  ! define element type
  type element
    integer :: label          ! identifier for element in question
    integer :: parent         ! the cell label to which the element belongs
    integer :: stage          ! (0) (1) (2) representing (fading in) (steady-state) (fading out)
    integer :: type           ! element type - e.g. cytoplasm (1), cortex (2)
    integer :: DIT_factor     ! Factor for setting adjustement of inter-element interactions according to differential adhesion hypothesis. This factor essentially labels the kind of interface at which an element sits.
    real    :: adhesion_factor! Factor for adjusting the magnitude of adhesion according to local density of surface elements.
    real*8  :: age            ! element age (an internal clock for each element)
    real*8  :: blebfactor     ! element age (an internal clock for each element)
    real*8  :: strength       ! a linear measure of presence/age for elements of status=0 (fading in)
    real*8, dimension(3) :: position ! element's position in absolute frame
    real*8, dimension(3) :: velocity ! element's instanteous velocity
    real*8, dimension(3) :: polar    ! element's position in polar coordinates relative to the cell centre of mass.
  end type element

  ! define cell type
  type cell
    integer :: label        ! identifier for cell in question
    integer :: fate         ! Label to determine cell fate. Fate = 1 for epiblast, fate =2 for hypoblast, fate =3 for trophoblast
    integer :: triplet_count! Number of element triplets in the Delaunay triangulation
    real*8  :: rad_gyration ! radius of gyration of the cell
    real*8  :: age          ! cell age
    real*8  :: volume       ! Value of cell volume
    real*8, dimension(3) :: position          ! Cell's center of mass position in absolute frame
    real*8, dimension(3) :: original_position ! The original position of the cell at its creation or the beginning of the simulation
    integer, dimension(0:4*ne_cell)      :: c_elements ! Cell's register of internal elements. Zeroth component gives number of elements in cell
    integer, dimension(0:2*ne_cell)      :: cortex_elements  ! List of labels of cortical elements in the cell. Zeroth component is the number of cortical elements. (Is ne_cell big enough???)
    integer, allocatable, dimension(:,:) :: triplets         ! Array of cortex element triplets in cell formed by Delaunay triangulation. Note that the values in these triplets are global element labels.
  end type cell

  type cortexpair
    integer :: label1         !Label of first element in pair
    integer :: label2         !Label of second element in pair
    real*8  :: cortex_factor  !Factor to determine magnitude of cortical tension at this pair
  end type cortexpair

  ! declare element and cell data arrays
  type(element), allocatable, dimension(:) :: elements        !Creates an array of allocatable dimension, for which each element is of the type "element",
  type(cell), allocatable, dimension(:) :: cells              !meaning that each element of the array has 8 pieces of data: label, parent, stage etc, and two of these are themselves 3D arrays.
                                                              !The same goes for the "cells" array. Each element is of type "cell" with several data components.
  type(cortexpair), allocatable, dimension(:) :: pairs_cortex !Array of cortex pairs for all interactions between cortex elements in all cells of the system.

end module scem_1_types
