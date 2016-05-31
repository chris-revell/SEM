! T. J Newman, Tempe, July 2010

! edited for growth algorithm, September 2010

module scem_0_input

  implicit none

  ! declare system parameters and constants

  integer, parameter :: dim=3 ! --> number of spatial dimensions
  integer, parameter :: ne_cell=128 ! --> number of elements per cell

  integer :: ne,nc,np ! numbers of elements, cells, and element pairs
  integer :: ne_size,nc_size,np_size,np_cortex ! parameters for array size allocations
  integer :: flag_relist ! flag triggering relist of sector assignments
  real*8  :: r_inflex ! inflexion point of potential - calculated in scem_inflexion module

  ! meaning of parameters given below when assigned values
  real*8, allocatable, dimension(:,:,:,:,:,:) :: rel_strength		!For interaction of element a in cell A with element b in cell B argument is (type of cell A, type of cell B, type of element a, type of element b)
  integer :: nx,ny,nz
  integer :: iseed,iloop1,iloop2,iloop3,iloop4,iloop5
  !system switches
  integer :: flag_create,flag_diffusion,flag_growth,flag_division,flag_conserve
  integer :: flag_background,flag_cortex,flag_DIT,flag_povray_elements
  integer :: flag_povray_pairs,flag_povray_volumes, flag_povray, flag_povray_triangles, flag_povray_cortex_pairs
  integer :: flag_count_output,flag_fate_output,flag_volume_output,flag_measure,flag_measure_radius,flag_measure_neighbours,&
                flag_elements_final
  integer :: n_c_types,n_e_types
  integer :: n_bins
  integer :: n_snapshots		!Number of system snapshots outputted to file "elements"
  real*8  :: p3,pi,ot
  real*8  :: r_cell,r_cell_sq
!  real*8 :: r_boundary			!Radius of confining spherical boundary
  real*8  :: viscous_timescale_cell,elastic_mod_cell,damping_cell,damping_element
  real*8  :: kappa_cell,kappa_element,epsilon,pot_min,rho,force_amplitude
  real*8  :: r_equil,r_equil_sq,frac_close,r_close_sq
  real*8  :: frac_interaction_max,r_interaction_max,r_interaction_max_sq,d_r_sq,d_r_sq_recip
  real*8  :: diff_coeff,diff_amp
  real*8  :: cell_cycle_time,rate_new_element,establishment_time,prob_new_element,frac_growth
  real*8  :: frac_placement_min,r_placement_min_sq
  real*8  :: buffer_frac,buffer_size,buffer_size_sq,sector_size,sector_size_sq,recip_sector_size
  real*8  :: time,time_out_1,time_max,dt,dt_amp_max,r_s_max !time_out_2,
  real*8  :: trigger_frac
  real*8  :: cortex_constant
  real*8  :: spherical_boundary_radius
  character(len=8) :: date_of_run   !Date of simulation run
  character(len=10) :: time_of_run   !Time of simulation run
  character(len=21):: output_folder !Name of folder created for data output, labelled according to date and time of run.
  integer :: start_time             !Variables for measuring time expired during simulation run
  integer :: current_time           !^
  integer :: total_system_time      !^
  real    :: count_rate             !^
  !Arrays for cortex element allocation
  integer, dimension(4,8,80)        :: bin_contents
  integer, dimension(4,8)           :: bin_counters

  real*8              :: h !Height of spherical cap boundary.
  real*8              :: cap_radius

  contains

    subroutine scem_input

      !Take time when run is initiated
      call SYSTEM_CLOCK(start_time, count_rate)

      !Create labelled file for data output
      !Catch date and time, create folder to store data in
      call date_and_time(DATE=date_of_run,TIME=time_of_run)
      output_folder = "../data/"//date_of_run//"_"//time_of_run
      call system("mkdir "//output_folder)
      call system("mkdir "//output_folder//"/system_data")
      call system("mkdir "//output_folder//"/povray_data")

	    n_snapshots = 0

!   Below used when cortical tension was specified at the command line.
!      CALL get_command_argument(1, arg)
!      read (arg,*) command_line_argument

      flag_create     = 0 ! flag_create = 0 (1) for initial cell from file (created de novo)
      flag_diffusion  = 1 ! flag_diffusion = 0 (1) for no diffusion (diffusion)
      flag_conserve   = 0 ! flag_conserve=1 (0) for volume conservation (no volume conservation)
      flag_background = 4 ! flag_background determines whether to use background potential, and if so which potential. =0 for no background potential, =1 for "test tube", =2 for spherical well
      flag_growth     = 1 ! flag_growth = 0 (1) for no growth (growth)
      flag_division   = 1 ! flag_division = 0 (1) for growth with no cell division (with cell division)
      flag_cortex     = 1 ! flag_cortex = 1 (0) to identify cortex elements (not identifying cortex elements) MUST ALWAYS BE SWITCHED ON IF VOLUME IS CALCULATED OR ELSE PROGRAM WILL FAIL AT RUN TIME
      flag_DIT        = 1 ! flag_DIT = 1 (0) for differential interfacial tension (no differential interfacial tension)

      !Output control flags
      flag_povray = 1          !switch to turn off povray output entirely
        flag_povray_volumes      = 1 ! flag_povray_volumes = 1 to output cell position data in povray format, 0 to skip.
        flag_povray_elements     = 1 ! flag_povray_elements = 1 to output element position data in povray format, 0 to skip.
        flag_povray_pairs        = 0 ! flag_povray_pairs = 1 to show interaction pairs as cylinders in povray output, 0 to skip.
        flag_povray_triangles    = 1 ! Switch to turn smoothed triangle povray output on and off.
        flag_povray_cortex_pairs = 1 ! Switch to turn Delaunay cortex interaction on and off
      flag_count_output       = 1    ! Switch to turn off outputting cell count
      flag_fate_output        = 0    ! Switch to turn off outputting cell fate data
      flag_volume_output      = 1    ! Switch to turn off outputting cell volume data
      flag_elements_final     = 0    ! Switch to turn off outputting elements_final data file.
      flag_measure            = 1    ! Switch to turn off element pair ratio sorting measurement
      flag_measure_radius     = 1    ! Switch to turn off radius difference sorting measurement
      flag_measure_neighbours = 1    ! Switch to turn off neighbour pair ratio sorting measurement

      ! numerical constants
      pi=4.0*atan(1.0) ! pi
      ot=1.0/3.0 ! one thid
      p3=pi/(3.0*sqrt(2.0)) ! packing fraction in 3D

      ! system parameters
      iseed=-6345 ! initial random number seed, originally -6345
      trigger_frac=0.5 ! safety margin for triggering array reallocation
      ! derived quantitites
      dt_amp_max=0.1*(1.0-0.875*flag_create) ! empirically found best values for time step amplitude

      ! cell parameters
      n_c_types=2 ! Number of cell types. 1=epliblast, 2=hypoblast
      n_e_types=2 ! Number of element types. 1=cytoplasm, 2=cortex
      r_cell=10.0 ! Radial spatial scale of one cell in *microns*
      ! derived quantities
      r_cell_sq=r_cell**2 ! radial scale squared
      allocate(rel_strength(2,0:n_c_types,0:n_c_types,0:n_e_types,0:n_e_types,2)) ! allocate rel_strength array

      ! physical parameters
      viscous_timescale_cell=100 ! time scale of viscous relaxation for cell in *seconds*
      elastic_mod_cell=1.0 ! elastic modulus of cell in *kiloPascals*
      ! derived quantities
      kappa_cell=elastic_mod_cell*r_cell ! effective spring constant for one cell in nanoNewtons/micron
      kappa_element=kappa_cell/(ne_cell+0.0)**ot ! effective Hookean spring constant between elements (assuming 3D)
      damping_cell=viscous_timescale_cell*kappa_cell ! effective damping constant for one cell
      damping_element=damping_cell/ne_cell ! effective damping constant for one element

      ! interactions
      ! here are parameters for intracellular element interactions
      ! all other pairwise interactions across element types will be scaled using a relative strength matrix
      ! length scales will be kept constant across all element types for the time being
      epsilon=0.01 ! --> value of potential relative to minimum at r_interaction_max
      n_bins=1024 ! --> number of bins for potential table
      frac_close=0.8 ! --> fraction of equilibrium distance for initialization (< approx 0.85)
      frac_interaction_max=1.8  !1.8 ! --> fraction of equilibrium distance for maximum interaction range (in window 1.5 to 1.9)
                                !     1.5 has sharply contracted potential and efficient
                                !     1.9 has more relaxed potential, and still reasonably efficient (factor of 2.5 slower)
                                !     > 1.9 brings in more element interactions, contracting cell, and slowing simulation
                                !     however, < 2.0 creates crystalline faceting with growth algorithms
        ! derived quantitites
        r_equil=2*r_cell*(p3/ne_cell)**ot ! equilibrium distance between elements (assuming 3D here)
        r_equil_sq=r_equil**2 ! square of equilibrium distance
        r_close_sq=(frac_close*r_equil)**2 ! square of minimum distance for initialization
        r_interaction_max=frac_interaction_max*r_equil ! maximum interaction range
        r_interaction_max_sq=r_interaction_max**2 ! square of maximum interaction range
        d_r_sq=r_interaction_max_sq/n_bins
        d_r_sq_recip=1.0/d_r_sq
        rho=-(1.0/(frac_interaction_max**2-1.0))*log(1.0-sqrt(1.0-epsilon)) ! value of rho to ensure V/V_min=epsilon
        pot_min=(kappa_element/8)*(r_equil/rho)**2 ! value of V_min to ensure correct spring constant
        force_amplitude=4.0*pot_min*rho/r_equil**2 ! prefactor of force expression

      ! diffusion parameters
      diff_coeff=0.001 ! --> diffusion coefficient of elements in units of micron^2/s

      ! growth parameters
      cell_cycle_time=4320*10			!0.5*3600.0 ! --> cell cycle time in seconds
      frac_growth=0.9 ! fraction of current cell radius within which new elements may be placed
      frac_placement_min=0.6 ! minimum separation of new element from nearest neighbour, as fraction of r_equil
        ! derived quantities
        rate_new_element=ne_cell/cell_cycle_time ! rate per cell at which new elements introduced
        establishment_time=1.0/rate_new_element ! transient time for each new element to become established ("faded in")
                                                ! note: may need better derivation for this time and relationship to cell_cycle_time
        r_placement_min_sq=(frac_placement_min*r_equil)**2 ! squared minimum distance from new element to nearest neighbour


	    !assign values to relative strength array
	    rel_strength(:,:,:,:,:,:)=0.0	!default interactions are zero
		  ! User supplies entries for relative strength "matrix"
		  ! Fill in values for pairwise interactions (i,j,k,l) with i < = j and k < = l.
		  ! Algorithm will then automatically fill in values for switching indices i and j, and k and l (tensor is symmetric under flipping of these indices.
		  !Note that it is only symmetric if both indices are flipped - it is not symmetrical under flipping of only one index)
		  !                                                     last index = 1 (intra-cellular interactions)
      !                                                                = 2 (inter-cellular interactions)

!     rel_strength(adhesive/repulsive,fate1,fate2,type1,type1,intra/inter)
		  rel_strength(1,1,1,1,1,1) = 1.0  !Adhesive component, intra-cellular Epiblast cytoplasm-epiblast cytoplasm
		  rel_strength(1,1,1,1,2,1) = 1.0	 !Adhesive component, intra-cellular Epiblast cytoplasm-epiblast cortex
      rel_strength(1,1,1,2,2,1)	= 1.0  !Adhesive component, intra-cellular Epiblast cortex-epiblast cortex
		  rel_strength(1,1,2,1,1,1)	= 0.0	 !Adhesive component, intra-cellular Epiblast cytoplasm-hypoblast cytoplasm. Set to zero but shouldn't happen anyway.
		  rel_strength(1,1,2,1,2,1) = 0.0	 !Adhesive component, intra-cellular Epiblast cytoplasm-hypoblast cortex. Set to zero but shouldn't happen anyway.
		  rel_strength(1,1,2,2,2,1) = 0.0	 !Adhesive component, intra-cellular Epiblast cortex-hypoblast cortex. Set to zero but shouldn't happen anyway.
		  rel_strength(1,2,2,1,1,1) = 1.0	 !Adhesive component, intra-cellular Hypoblast cytoplasm-hypoblast cytoplasm
		  rel_strength(1,2,2,1,2,1) = 1.0	 !Adhesive component, intra-cellular Hypoblast cytoplasm-hypoblast cortex
		  rel_strength(1,2,2,2,2,1) = 1.0	 !Adhesive component, intra-cellular Hypoblast cortex-hypoblast cortex

		  rel_strength(1,1,1,1,1,2) = 0.0  !Adhesive component, inter-cellular Epiblast cytoplasm-epiblast cytoplasm
		  rel_strength(1,1,1,1,2,2) = 0.0  !Adhesive component, inter-cellular Epiblast cytoplasm-epiblast cortex
		  rel_strength(1,1,1,2,2,2) = 0.0  !Adhesive component, inter-cellular Epiblast cortex-epiblast cortex
  		rel_strength(1,1,2,1,1,2) = 0.0  !Adhesive component, inter-cellular Epiblast cytoplasm-hypoblast cytoplasm
  		rel_strength(1,1,2,1,2,2) = 0.0  !Adhesive component, inter-cellular Epiblast cytoplasm-hypoblast cortex
  		rel_strength(1,1,2,2,2,2) = 0.0  !Adhesive component, inter-cellular Epiblast cortex-hypoblast cortex
  		rel_strength(1,2,2,1,1,2) = 0.0  !Adhesive component, inter-cellular Hypoblast cytoplasm-hypoblast cytoplasm
  		rel_strength(1,2,2,1,2,2) = 0.0  !Adhesive component, inter-cellular Hypoblast cytoplasm-hypoblast cortex
  		rel_strength(1,2,2,2,2,2) = 0.0  !Adhesive component, inter-cellular Hypoblast cortex-hypoblast cortex

      rel_strength(2,1,1,1,1,1) = 1.0  !Repulsive component, intra-cellular Epiblast cytoplasm-epiblast cytoplasm
		  rel_strength(2,1,1,1,2,1) = 1.0	 !Repulsive component, intra-cellular Epiblast cytoplasm-epiblast cortex
      rel_strength(2,1,1,2,2,1)	= 1.0  !Repulsive component, intra-cellular Epiblast cortex-epiblast cortex
		  rel_strength(2,1,2,1,1,1)	= 0.0	 !Repulsive component, intra-cellular Epiblast cytoplasm-hypoblast cytoplasm. Set to zero but shouldn't happen anyway.
		  rel_strength(2,1,2,1,2,1) = 0.0	 !Repulsive component, intra-cellular Epiblast cytoplasm-hypoblast cortex. Set to zero but shouldn't happen anyway.
		  rel_strength(2,1,2,2,2,1) = 0.0	 !Repulsive component, intra-cellular Epiblast cortex-hypoblast cortex. Set to zero but shouldn't happen anyway.
		  rel_strength(2,2,2,1,1,1) = 1.0	 !Repulsive component, intra-cellular Hypoblast cytoplasm-hypoblast cytoplasm
		  rel_strength(2,2,2,1,2,1) = 1.0	 !Repulsive component, intra-cellular Hypoblast cytoplasm-hypoblast cortex
		  rel_strength(2,2,2,2,2,1) = 1.0	 !Repulsive component, intra-cellular Hypoblast cortex-hypoblast cortex

		  rel_strength(2,1,1,1,1,2) = 1.0  !Repulsive component, inter-cellular Epiblast cytoplasm-epiblast cytoplasm
		  rel_strength(2,1,1,1,2,2) = 1.0  !Repulsive component, inter-cellular Epiblast cytoplasm-epiblast cortex
		  rel_strength(2,1,1,2,2,2) = 1.0  !Repulsive component, inter-cellular Epiblast cortex-epiblast cortex
  		rel_strength(2,1,2,1,1,2) = 1.0  !Repulsive component, inter-cellular Epiblast cytoplasm-hypoblast cytoplasm
  		rel_strength(2,1,2,1,2,2) = 1.0  !Repulsive component, inter-cellular Epiblast cytoplasm-hypoblast cortex
  		rel_strength(2,1,2,2,2,2) = 1.0  !Repulsive component, inter-cellular Epiblast cortex-hypoblast cortex
  		rel_strength(2,2,2,1,1,2) = 1.0  !Repulsive component, inter-cellular Hypoblast cytoplasm-hypoblast cytoplasm
  		rel_strength(2,2,2,1,2,2) = 1.0  !Repulsive component, inter-cellular Hypoblast cytoplasm-hypoblast cortex
  		rel_strength(2,2,2,2,2,2) = 1.0  !Repulsive component, inter-cellular Hypoblast cortex-hypoblast cortex

      r_s_max = MAXVAL(rel_strength)

      !Variable for inter-cortex potential
      cortex_constant = 0.01

      dt_amp_max=dt_amp_max/r_s_max ! rescale dt by largest interaction strength to ensure stable integration

      ! temporal parameters - all in *seconds*
      time_max=1.0*cell_cycle_time ! --> time of simulation in seconds
      time_out_1=time_max/99.0 ! --> interval between graphical data outputs, set such that there will be no more than 99 outputs regardless of time_max
!     time_out_2=cell_cycle_time/100.0 ! --> interval between quantitative data outputs
      dt=dt_amp_max*viscous_timescale_cell/(ne_cell+0.0)**(2*ot) ! --> optimized microscopic time increment
        ! derived quantities
        diff_amp=sqrt(dt*diff_coeff) ! amplitude of noise in diffusion term
        prob_new_element=rate_new_element*dt ! probability of growth for each time step

      ! sector parameters (used to highly optimize location of near neighbor elements)
      buffer_frac=0.1 ! fractional size of buffer for lifetime of neighbor table (sweet spot between 0.08-0.15)
        ! derived quantities
        buffer_size=buffer_frac*r_interaction_max ! absolute width of buffer region
        buffer_size_sq=buffer_size**2 ! square of buffer size for efficiency in module scem_flag_relist
        sector_size=0.5*(1.0+buffer_frac)*r_interaction_max ! sector size is max interaction range plus buffer
        sector_size_sq=sector_size**2 ! square of sector_size for efficiency in scem_2_pairs
        recip_sector_size=1.0/sector_size ! calculate reciprocal of sector size for efficiency
        nx=8*(2+int(4*r_cell/sector_size)) ! --> number of sectors in x direction
        ny=nx ! --> isotropic choice
        if (dim.eq.3) nz=nx ! --> isotropic choice in 3D
        if (dim.eq.2) nz=1 ! --> a single layer of sectors for 2D simulations

      spherical_boundary_radius = 0

    end subroutine scem_input

end module scem_0_input
