! T. J Newman, Tempe, July 2010

module scem_0_arrays

  implicit none

  ! declare system arrays
  ! note, the relative strength array is declared and allocated in the user-input module
  real*8, allocatable, dimension(:,:)			      :: potential_deriv        !potential_deriv tables are filled with gradient of inter-element potentials (ie force)
  real*8, dimension(3)							            :: x_cen
  real*8, allocatable, dimension(:,:)			      :: xe_compare
  real*8, allocatable, dimension(:,:)			      :: xe_prev
  integer, allocatable, dimension(:,:,:)	     	:: head
  integer, allocatable, dimension(:)	     		  :: list
  integer, allocatable, dimension(:,:)    			:: pairs
  integer, allocatable, dimension(:,:)          :: pairs_cortex     !Array containing pairs of element labels defining cortical tension network.
  integer, allocatable, dimension(:)		      	:: read_cell_fate   !Initial fate of cells read from file
  integer, allocatable, dimension(:,:)		    	:: neighbours
  integer, dimension(4,8)                       :: max_radius_elements  !For use in scem_cortex

end module scem_0_arrays
