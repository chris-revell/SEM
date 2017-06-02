! T. J Newman, Tempe, July 2010

module scem_0_arrays

  implicit none

  ! declare system arrays
  ! note, the relative strength array is declared and allocated in the user-input module
  real*8, allocatable, dimension(:,:)			 :: potential_deriv1       !potential_deriv tables are filled with gradient of inter-element potentials (ie force). potential_deriv1 contains the attractive component.
  real*8, allocatable, dimension(:,:)			 :: potential_deriv2       !potential_deriv tables are filled with gradient of inter-element potentials (ie force). potential_deriv2 contains the repulsive component.
  real*8, dimension(3)							       :: x_cen
  real*8, allocatable, dimension(:,:)			 :: xe_compare
  real*8, allocatable, dimension(:,:)			 :: xe_prev
  real*8, dimension(3)                     :: pre_com, epi_com
  integer, allocatable, dimension(:,:,:)	 :: head
  integer, allocatable, dimension(:)	     :: list
  integer, allocatable, dimension(:,:)     :: pairs
  integer, dimension(200,200)		           :: neighbours
  integer, dimension(32,256)               :: bin_contents    !For use in scem_cortex
  integer, dimension(32)                   :: bin_counters    !For use in scem_cortex
  real*8, dimension(32)                    :: bin_max_radius  !For use in scem_cortex
  integer,dimension(200)                   :: stored_fates  !For use in scem_measure_randomised
  integer,dimension(20000)                 :: tested

end module scem_0_arrays
