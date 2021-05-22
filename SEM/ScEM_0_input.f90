! T. J Newman, Tempe, July 2010

module scem_0_input

  implicit none

  !System parameters and constants
  integer, parameter :: ne_cell=128 !Number of elements per cell
  integer :: ne,nc,np !Numbers of elements, cells, and element pairs
  integer :: ne_size,nc_size,np_size,np_cortex !Parameters for array size allocations
  integer :: n_c_types,n_e_types
  integer :: nc_initial                        !Number of cells at which to start measuring system when greating system de novo
  integer :: n_bins
  integer :: n_snapshots		!Number of system snapshots outputted to file "elements"
  real*8  :: r_inflex ! inflexion point of potential - calculated in scem_inflexion module
  real*8, allocatable, dimension(:,:,:,:,:,:) :: rel_strength		!For interaction of element a in cell A with element b in cell B argument is (type of cell A, type of cell B, type of element a, type of element b)
  real*8, allocatable, dimension(:,:,:,:,:,:) :: intro_rel_strength
  integer :: nx,ny,nz
  integer :: iloop1,iloop2,iloop3,iloop4,iloop5
  !System switches
  integer :: flag_create,flag_diffusion,flag_growth,flag_division,flag_conserve
  integer :: flag_background,flag_povray_elements, flag_randomise
  integer :: flag_povray_pairs,flag_povray_volumes,flag_povray,flag_povray_triangles,flag_povray_cortex_pairs,flag_povray_boundary
  integer :: flag_count_output,flag_fate_output,flag_volume_output,flag_measure_radius
  integer :: flag_measure_neighbours,flag_measure_displacement,flag_measure_surface,flag_elements_final,flag_symmetric_division
  integer :: flag_measure_randomised,flag_measure_velocity,flag_measure_com,flag_random_init,flag_pre_blebbing
  integer :: flag_relist ! flag triggering relist of sector assignments
  !Variables for initiating randoms number sequence
  integer :: seedarraylength
  integer, allocatable, dimension(:) :: seed_array
  !Variables for measuring time expired during simulation run
  integer :: start_time
  integer :: current_time
  integer :: total_system_time
  real    :: count_rate
  !Numerical parameters
  real*8  :: p3,pi,ot
  real*8  :: r_cell,r_cell_sq
  real*8  :: viscous_timescale_cell,elastic_mod_cell,damping_cell,damping_element
  real*8  :: kappa_cell,kappa_element,epsilon,pot_min,rho,force_amplitude
  real*8  :: r_equil,r_equil_sq,frac_close,r_close_sq
  real*8  :: frac_interaction_max,r_interaction_max,r_interaction_max_sq,d_r_sq,d_r_sq_recip
  real*8  :: diff_coeff,diff_amp
  real*8  :: cell_cycle_time,rate_new_element,establishment_time,prob_new_element,frac_growth
  real*8  :: frac_placement_min,r_placement_min_sq
  real*8  :: buffer_frac,buffer_size,buffer_size_sq,sector_size,sector_size_sq,recip_sector_size
  real*8  :: time,output_interval,output_interval2,time_max,dt,dt_amp_max,r_s_max
  real*8  :: trigger_frac
  !Variables for cell behaviour
  real*8 :: stiffness_factor
  real*8 :: cortex_constant1,cortex_constant2
  real*8,dimension(2,0:2) :: DIT_response
  real*8 :: epi_adhesion
  real*8 :: pre_adhesion
  real*8 :: epi_pre_adhesion
  real   :: n_cellcycles !Number of cell cycles for run

  !Variables for setting output folder
  character(len=24):: output_folder !Name of folder created for data output, labelled according to date and time of run.
  !Variables defined for command line input
  character(len=4) :: arg1,arg2,arg3,arg4
  character(len=1) :: arg0,arg5

  logical :: randomising
  logical :: intro

!**********
  real*8 :: h !Height of spherical cap boundary.
  real*8 :: cap_radius
  real*8 :: spherical_boundary_radius
!**********

  integer :: n_random     = 0
  integer :: n_random_max = 10000

  real*8  :: area_normalisation_factor
  integer :: area_normalisation_count
  real*8  :: bleb_amp
  integer	:: neighbour_epi_measurement,neighbour_pre_measurement
  real*8  :: surface_epi_measurement,surface_pre_measurement
  real*8  :: epiradius_measurement,preradius_measurement
  real*8  :: sysepiradius_measurement,syspreradius_measurement
  integer :: neighbour_epi_below,surface_epi_above
  integer :: neighbour_pre_above,surface_pre_below
  integer :: radius_pre_below,radius_pre_sys_below
  integer :: radius_epi_above,radius_epi_sys_above
  integer :: epielementcount
  integer :: epicellcount

  real*8 :: rad_max
  real*8 :: surface_max
  integer:: epineighbourmax

  integer:: ran_loop

  contains

    subroutine scem_input

      !Simulation control switches
      flag_create      = 1 ! flag_create = 0 (1) for initial cell from file (created de novo)
      flag_random_init = 1
      flag_diffusion   = 1 ! flag_diffusion = 0 (1) for no diffusion (diffusion)
      flag_conserve    = 0 ! flag_conserve=1 (0) for volume conservation (no volume conservation)
      flag_background  = 1 ! flag_background determines whether to use background potential, and if so which potential. =0 for no background potential, =1 for "test tube", =2 for spherical well
      flag_growth      = 1 ! flag_growth = 0 (1) for no growth (growth)
      flag_division    = 1 ! flag_division = 0 (1) for growth with no cell division (with cell division)
      flag_randomise   = 0 ! When importing initial system setup from file, if flag_randomise=1, the program will assign fates to the imported cells randomly rather than keeping the initial fate distribution
      flag_pre_blebbing= 1 ! Causes blebbing in primitive endoderm (cell type 2) when equal to 1.

      !Output control switches
      flag_povray = 1                ! Switch to turn off povray output entirely
        flag_povray_volumes      = 0 ! flag_povray_volumes = 1 to output cell position data in povray format, 0 to skip.
        flag_povray_elements     = 0 ! flag_povray_elements = 1 to output element position data in povray format, 0 to skip.
        flag_povray_pairs        = 0 ! flag_povray_pairs = 1 to show interaction pairs as cylinders in povray output, 0 to skip.
        flag_povray_triangles    = 1 ! Switch to turn smoothed triangle povray output on and off.
        flag_povray_cortex_pairs = 0 ! Switch to turn Delaunay cortex interaction on and off
        flag_povray_boundary     = 0 ! Switch to turn on boundary visualisation.
      flag_count_output       = 0    ! Switch to turn off outputting cell count
      flag_fate_output        = 0    ! Switch to turn off outputting cell fate data
      flag_volume_output      = 1    ! Switch to turn off outputting cell volume data
      flag_elements_final     = 0    ! Switch to turn off outputting elements_final data file.
      flag_measure_radius     = 1    ! Switch to turn off radius difference sorting measurement
      flag_measure_neighbours = 1    ! Switch to turn off neighbour pair ratio sorting measurement
      flag_measure_displacement=0    ! Switch to turn off displacement sorting measurement
      flag_measure_surface    = 1    ! Switch to turn off surface sorting measurement
      flag_measure_velocity   = 0    ! Switch to turn off velocity measurement
      flag_measure_com        = 0
      flag_measure_randomised = 1    ! Switch for subroutine that randomises fates in system and takes measurements as a baseline comparison

      !Simulation control parameters
      nc_initial        = 10
      stiffness_factor  = 1.0
      cell_cycle_time   = 1500.0 ! Cell cycle time in seconds
      n_cellcycles      = 2.3

      CALL GET_COMMAND_ARGUMENT(1,arg0)
      READ(arg0,*) flag_symmetric_division ! If flag_symmetric_division=1, division will always produce daughter cells of the same fate as the parent cell.
      CALL GET_COMMAND_ARGUMENT(2,arg1)
      READ(arg1,*) epi_adhesion            ! Magnitude of mutual adhesion between epiblasts (type 1)
      epi_adhesion     = epi_adhesion
      pre_adhesion     = epi_adhesion      ! Magnitude of mutual adhesion between primitive endoderm (type 2)
      epi_pre_adhesion = pre_adhesion      ! Magnitude of adhesion between epiblasts and primitive endoderm
      CALL GET_COMMAND_ARGUMENT(3,arg2)
      READ(arg2,*) cortex_constant1        ! Magnitude of baseline cortical tension in epiblasts
      cortex_constant2  = cortex_constant1 ! Magnitude of baseline cortical tension in primitive endoderm
      DIT_response(1,0) = 1.0              ! Epiblast external system surface DIT response factor
      CALL GET_COMMAND_ARGUMENT(4,arg3)
      READ(arg3,*) DIT_response(1,1)       ! Epiblast homotypic interface DIT response factor
      DIT_response(1,2) = 1.0              ! Epiblast heterotypic interface DIT response factor
      DIT_response(2,0) = 1.0              ! Primitive endoderm external system surface DIT response factor
      DIT_response(2,1) = 1.0              ! Primitive endoderm homotypic interface DIT response factor
      DIT_response(2,2) = 1.0              ! Primitive endoderm heterotypic interface DIT response factor
      CALL GET_COMMAND_ARGUMENT(5,arg4)
      READ(arg4,*) bleb_amp                ! Blebbing amplitude

      ! *** Everything from here on can effectively be ignored for the purposes of testing simulation parameters ***

      if (flag_random_init.EQ.1) then
      !Use processor determined random number seed and print that seed
        call RANDOM_SEED
        call RANDOM_SEED(size=seedarraylength)
        allocate(seed_array(seedarraylength))
        call RANDOM_SEED(get=seed_array)
        print*, "seed_array", seed_array
      else
      !Use user-defined random number seed:
        allocate(seed_array(2))
        seed_array(1) = 1591826533
        seed_array(2) = 497
        call RANDOM_SEED(PUT=seed_array)
      endif

      !Take time when run is initiated
      call SYSTEM_CLOCK(start_time, count_rate)

      !Create labelled file for data output
      !Catch date and time, create folder to store data in
      CALL GET_COMMAND_ARGUMENT(6,arg5)
      !call date_and_time(DATE=date_of_run,TIME=time_of_run)
      output_folder = "data/"//arg0//"_"//arg1(1:2)//arg1(4:)//"_"//arg2(1:1)//arg2(3:4)//"_"//arg3(1:1)//arg3(3:4)//"_"//&
        arg4(1:1)//arg4(3:4)//"_"//arg5(1:1)
      call system("mkdir "//output_folder)
      call system("mkdir "//output_folder//"/system_data")
      call system("mkdir "//output_folder//"/sorting_data")
      if (flag_povray.EQ.1)             call system("mkdir "//output_folder//"/povray_data")
      if (flag_measure_randomised.EQ.1) call system("mkdir "//output_folder//"/randomised_data")

      randomising = .FALSE.

      ! numerical constants
      pi=4.0*atan(1.0) ! pi
      ot=1.0/3.0 ! one third
      p3=pi/(3.0*sqrt(2.0)) ! packing fraction in 3D

      ! system parameters
      trigger_frac=0.5 ! safety margin for triggering array reallocation
      ! derived quantitites
      dt_amp_max=0.1
      ! cell parameters
      n_c_types=2 ! Number of cell types. 1=epiblast, 2=hypoblast
      n_e_types=2 ! Number of element types. 1=cytoplasm, 2=cortex
        !r_cell is a general lengthscale to set the equilibrium distance for interactions and the number of sectors.
        !Do we really need it? can we make do without? Is it needed to give a grounding in real units?
      r_cell    = 10.0 ! Radial spatial scale of one cell in *microns*
      r_cell_sq = r_cell**2 ! radial scale squared

      ! physical parameters
      ! viscous_timescale_cell used to set timestep length and damping_cell
      viscous_timescale_cell=100 ! time scale of viscous relaxation for cell in *seconds*
      !elastic_mod_cell used to set spring constant kappa_cell, and hence kappa_element and damping constants damping_cell and damping_element
      elastic_mod_cell=1.0 ! elastic modulus of cell in *kiloPascals*
      ! derived quantities
      kappa_cell=elastic_mod_cell*r_cell ! effective spring constant for one cell in nanoNewtons/micron. Only used in the next line.
      kappa_element=kappa_cell/(ne_cell+0.0)**ot ! effective Hookean spring constant between elements (assuming 3D)
      damping_cell=viscous_timescale_cell*kappa_cell ! effective damping constant for one cell
      damping_element=damping_cell/ne_cell ! effective damping constant for one element
      !damping_element used in scem_1_potential. potential_deriv1=potential_deriv1/damping_element and potential_deriv2=potential_deriv2/damping_element
      !Thus the value of damping_element directly affects the velocity given to an element by an interaction. Higher damping_element leads to lower velocities.

      ! Interactions:
      epsilon=0.01   ! value of potential relative to minimum at r_interaction_max
      n_bins=1024    ! number of bins for potential table

      frac_interaction_max=1.8  !1.8 ! --> fraction of equilibrium distance for maximum interaction range (in window 1.5 to 1.9)
                                !     1.5 has sharply contracted potential and efficient
                                !     1.9 has more relaxed potential, and still reasonably efficient (factor of 2.5 slower)
                                !     > 1.9 brings in more element interactions, contracting cell, and slowing simulation
                                !     however, < 2.0 creates crystalline faceting with growth algorithms
        ! derived quantitites
        r_equil=2*r_cell*(p3/ne_cell)**ot ! equilibrium distance between elements (assuming 3D here)
        r_equil_sq=r_equil**2 ! square of equilibrium distance

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
      frac_growth=0.9 ! fraction of current cell radius within which new elements may be placed
        ! derived quantities
        rate_new_element=ne_cell/cell_cycle_time ! rate per cell at which new elements introduced
        establishment_time=1.0/rate_new_element ! transient time for each new element to become established ("faded in")
                                                ! note: may need better derivation for this time and relationship to cell_cycle_time


      ! Define rel_strength array
      ! Explanation of indices:
      ! rel_strength(adhesive/repulsive,fate1,fate2,type1,type1,intra/inter)
      allocate(rel_strength(2,0:n_c_types,0:n_c_types,0:n_e_types,0:n_e_types,2))
      rel_strength(:,:,:,:,:,:) = 0
      rel_strength(1,1,1,1,1,1) = stiffness_factor  !Adhesive component, intra-cellular Epiblast cytoplasm-epiblast cytoplasm
		  rel_strength(1,1,1,1,2,1) = stiffness_factor	 !Adhesive component, intra-cellular Epiblast cytoplasm-epiblast cortex
      rel_strength(1,1,1,2,2,1)	= stiffness_factor  !Adhesive component, intra-cellular Epiblast cortex-epiblast cortex
		  rel_strength(1,1,2,1,1,1)	= 0.0	 !Adhesive component, intra-cellular Epiblast cytoplasm-hypoblast cytoplasm. Set to zero but shouldn't happen anyway.
		  rel_strength(1,1,2,1,2,1) = 0.0	 !Adhesive component, intra-cellular Epiblast cytoplasm-hypoblast cortex. Set to zero but shouldn't happen anyway.
		  rel_strength(1,1,2,2,2,1) = 0.0	 !Adhesive component, intra-cellular Epiblast cortex-hypoblast cortex. Set to zero but shouldn't happen anyway.
		  rel_strength(1,2,2,1,1,1) = stiffness_factor	 !Adhesive component, intra-cellular Hypoblast cytoplasm-hypoblast cytoplasm
		  rel_strength(1,2,2,1,2,1) = stiffness_factor	 !Adhesive component, intra-cellular Hypoblast cytoplasm-hypoblast cortex
		  rel_strength(1,2,2,2,2,1) = stiffness_factor	 !Adhesive component, intra-cellular Hypoblast cortex-hypoblast cortex

		  rel_strength(1,1,1,1,1,2) = 0.0  !Adhesive component, inter-cellular Epiblast cytoplasm-epiblast cytoplasm
		  rel_strength(1,1,1,1,2,2) = 0.0  !Adhesive component, inter-cellular Epiblast cytoplasm-epiblast cortex
      rel_strength(1,1,1,2,2,2) = epi_adhesion  !Adhesive component, inter-cellular Epiblast cortex-epiblast cortex
  		rel_strength(1,1,2,1,1,2) = 0.0  !Adhesive component, inter-cellular Epiblast cytoplasm-hypoblast cytoplasm
  		rel_strength(1,1,2,1,2,2) = 0.0  !Adhesive component, inter-cellular Epiblast cytoplasm-hypoblast cortex
  		rel_strength(1,1,2,2,2,2) = epi_pre_adhesion  !Adhesive component, inter-cellular Epiblast cortex-hypoblast cortex
  		rel_strength(1,2,2,1,1,2) = 0.0  !Adhesive component, inter-cellular Hypoblast cytoplasm-hypoblast cytoplasm
  		rel_strength(1,2,2,1,2,2) = 0.0  !Adhesive component, inter-cellular Hypoblast cytoplasm-hypoblast cortex
  		rel_strength(1,2,2,2,2,2) = pre_adhesion  !Adhesive component, inter-cellular Hypoblast cortex-hypoblast cortex

      rel_strength(2,1,1,1,1,1) = stiffness_factor  !Repulsive component, intra-cellular Epiblast cytoplasm-epiblast cytoplasm
		  rel_strength(2,1,1,1,2,1) = stiffness_factor	!Repulsive component, intra-cellular Epiblast cytoplasm-epiblast cortex
      rel_strength(2,1,1,2,2,1)	= stiffness_factor  !Repulsive component, intra-cellular Epiblast cortex-epiblast cortex
		  rel_strength(2,1,2,1,1,1)	= 0.0	 !Repulsive component, intra-cellular Epiblast cytoplasm-hypoblast cytoplasm. Set to zero but shouldn't happen anyway.
		  rel_strength(2,1,2,1,2,1) = 0.0	 !Repulsive component, intra-cellular Epiblast cytoplasm-hypoblast cortex. Set to zero but shouldn't happen anyway.
		  rel_strength(2,1,2,2,2,1) = 0.0	 !Repulsive component, intra-cellular Epiblast cortex-hypoblast cortex. Set to zero but shouldn't happen anyway.
		  rel_strength(2,2,2,1,1,1) = stiffness_factor	 !Repulsive component, intra-cellular Hypoblast cytoplasm-hypoblast cytoplasm
		  rel_strength(2,2,2,1,2,1) = stiffness_factor	 !Repulsive component, intra-cellular Hypoblast cytoplasm-hypoblast cortex
		  rel_strength(2,2,2,2,2,1) = stiffness_factor	 !Repulsive component, intra-cellular Hypoblast cortex-hypoblast cortex

		  rel_strength(2,1,1,1,1,2) = 4.0*stiffness_factor  !Repulsive component, inter-cellular Epiblast cytoplasm-epiblast cytoplasm
		  rel_strength(2,1,1,1,2,2) = 4.0*stiffness_factor  !Repulsive component, inter-cellular Epiblast cytoplasm-epiblast cortex
		  rel_strength(2,1,1,2,2,2) = MAX(0.5*stiffness_factor,0.4*epi_adhesion)  !Repulsive component, inter-cellular Epiblast cortex-epiblast cortex
  		rel_strength(2,1,2,1,1,2) = 4.0*stiffness_factor  !Repulsive component, inter-cellular Epiblast cytoplasm-hypoblast cytoplasm
  		rel_strength(2,1,2,1,2,2) = 4.0*stiffness_factor  !Repulsive component, inter-cellular Epiblast cytoplasm-hypoblast cortex
  		rel_strength(2,1,2,2,2,2) = MAX(0.5*stiffness_factor,0.4*epi_pre_adhesion)  !Repulsive component, inter-cellular Epiblast cortex-hypoblast cortex
  		rel_strength(2,2,2,1,1,2) = 4.0*stiffness_factor  !Repulsive component, inter-cellular Hypoblast cytoplasm-hypoblast cytoplasm
  		rel_strength(2,2,2,1,2,2) = 4.0*stiffness_factor  !Repulsive component, inter-cellular Hypoblast cytoplasm-hypoblast cortex
  		rel_strength(2,2,2,2,2,2) = MAX(0.5*stiffness_factor,0.4*pre_adhesion)  !Repulsive component, inter-cellular Hypoblast cortex-hypoblast cortex

      r_s_max = MAXVAL(rel_strength)
      dt_amp_max=dt_amp_max/MAX(0.2*cortex_constant1/0.03357,r_s_max) ! rescale dt by largest interaction strength to ensure stable integration
                                    ! Note that this slows the system down significantly for higher interaction strengths. Is this really necessary?

      ! temporal parameters - all in *seconds*
      time_max=n_cellcycles*cell_cycle_time ! --> time of simulation in seconds
      output_interval=time_max/99.0 ! --> interval between graphical data outputs, set such that there will be no more than 99 outputs regardless of time_max
      output_interval2=output_interval
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
        nz=nx ! --> isotropic choice in 3D

!************
!These might be better off elsewhere
      spherical_boundary_radius = 0
      cap_radius                = 0
      n_snapshots = 0
!************

      allocate(intro_rel_strength(2,0:n_c_types,0:n_c_types,0:n_e_types,0:n_e_types,2))
      intro_rel_strength(1,1,1,1,1,1) = stiffness_factor  !Adhesive component, intra-cellular Epiblast cytoplasm-epiblast cytoplasm
      intro_rel_strength(1,1,1,1,2,1) = stiffness_factor	 !Adhesive component, intra-cellular Epiblast cytoplasm-epiblast cortex
      intro_rel_strength(1,1,1,2,2,1)	= stiffness_factor  !Adhesive component, intra-cellular Epiblast cortex-epiblast cortex
      intro_rel_strength(1,1,2,1,1,1)	= 0.0	 !Adhesive component, intra-cellular Epiblast cytoplasm-hypoblast cytoplasm. Set to zero but shouldn't happen anyway.
      intro_rel_strength(1,1,2,1,2,1) = 0.0	 !Adhesive component, intra-cellular Epiblast cytoplasm-hypoblast cortex. Set to zero but shouldn't happen anyway.
      intro_rel_strength(1,1,2,2,2,1) = 0.0	 !Adhesive component, intra-cellular Epiblast cortex-hypoblast cortex. Set to zero but shouldn't happen anyway.
      intro_rel_strength(1,2,2,1,1,1) = stiffness_factor	 !Adhesive component, intra-cellular Hypoblast cytoplasm-hypoblast cytoplasm
      intro_rel_strength(1,2,2,1,2,1) = stiffness_factor	 !Adhesive component, intra-cellular Hypoblast cytoplasm-hypoblast cortex
      intro_rel_strength(1,2,2,2,2,1) = stiffness_factor	 !Adhesive component, intra-cellular Hypoblast cortex-hypoblast cortex

      intro_rel_strength(1,1,1,1,1,2) = 0.0  !Adhesive component, inter-cellular Epiblast cytoplasm-epiblast cytoplasm
      intro_rel_strength(1,1,1,1,2,2) = 0.0  !Adhesive component, inter-cellular Epiblast cytoplasm-epiblast cortex
      intro_rel_strength(1,1,1,2,2,2) = epi_adhesion  !Adhesive component, inter-cellular Epiblast cortex-epiblast cortex
      intro_rel_strength(1,1,2,1,1,2) = 0.0  !Adhesive component, inter-cellular Epiblast cytoplasm-hypoblast cytoplasm
      intro_rel_strength(1,1,2,1,2,2) = 0.0  !Adhesive component, inter-cellular Epiblast cytoplasm-hypoblast cortex
      intro_rel_strength(1,1,2,2,2,2) = epi_adhesion  !Adhesive component, inter-cellular Epiblast cortex-hypoblast cortex
      intro_rel_strength(1,2,2,1,1,2) = 0.0  !Adhesive component, inter-cellular Hypoblast cytoplasm-hypoblast cytoplasm
      intro_rel_strength(1,2,2,1,2,2) = 0.0  !Adhesive component, inter-cellular Hypoblast cytoplasm-hypoblast cortex
      intro_rel_strength(1,2,2,2,2,2) = epi_adhesion  !Adhesive component, inter-cellular Hypoblast cortex-hypoblast cortex

      intro_rel_strength(2,1,1,1,1,1) = stiffness_factor  !Repulsive component, intra-cellular Epiblast cytoplasm-epiblast cytoplasm
      intro_rel_strength(2,1,1,1,2,1) = stiffness_factor	!Repulsive component, intra-cellular Epiblast cytoplasm-epiblast cortex
      intro_rel_strength(2,1,1,2,2,1)	= stiffness_factor  !Repulsive component, intra-cellular Epiblast cortex-epiblast cortex
      intro_rel_strength(2,1,2,1,1,1)	= 0.0	 !Repulsive component, intra-cellular Epiblast cytoplasm-hypoblast cytoplasm. Set to zero but shouldn't happen anyway.
      intro_rel_strength(2,1,2,1,2,1) = 0.0	 !Repulsive component, intra-cellular Epiblast cytoplasm-hypoblast cortex. Set to zero but shouldn't happen anyway.
      intro_rel_strength(2,1,2,2,2,1) = 0.0	 !Repulsive component, intra-cellular Epiblast cortex-hypoblast cortex. Set to zero but shouldn't happen anyway.
      intro_rel_strength(2,2,2,1,1,1) = stiffness_factor	 !Repulsive component, intra-cellular Hypoblast cytoplasm-hypoblast cytoplasm
      intro_rel_strength(2,2,2,1,2,1) = stiffness_factor	 !Repulsive component, intra-cellular Hypoblast cytoplasm-hypoblast cortex
      intro_rel_strength(2,2,2,2,2,1) = stiffness_factor	 !Repulsive component, intra-cellular Hypoblast cortex-hypoblast cortex

      intro_rel_strength(2,1,1,1,1,2) = 4.0*stiffness_factor  !Repulsive component, inter-cellular Epiblast cytoplasm-epiblast cytoplasm
		  intro_rel_strength(2,1,1,1,2,2) = 4.0*stiffness_factor  !Repulsive component, inter-cellular Epiblast cytoplasm-epiblast cortex
		  intro_rel_strength(2,1,1,2,2,2) = MAX(0.5*stiffness_factor,0.4*epi_adhesion)  !Repulsive component, inter-cellular Epiblast cortex-epiblast cortex
  		intro_rel_strength(2,1,2,1,1,2) = 4.0*stiffness_factor  !Repulsive component, inter-cellular Epiblast cytoplasm-hypoblast cytoplasm
  		intro_rel_strength(2,1,2,1,2,2) = 4.0*stiffness_factor  !Repulsive component, inter-cellular Epiblast cytoplasm-hypoblast cortex
  		intro_rel_strength(2,1,2,2,2,2) = MAX(0.5*stiffness_factor,0.4*epi_adhesion)  !Repulsive component, inter-cellular Epiblast cortex-hypoblast cortex
  		intro_rel_strength(2,2,2,1,1,2) = 4.0*stiffness_factor  !Repulsive component, inter-cellular Hypoblast cytoplasm-hypoblast cytoplasm
  		intro_rel_strength(2,2,2,1,2,2) = 4.0*stiffness_factor  !Repulsive component, inter-cellular Hypoblast cytoplasm-hypoblast cortex
  		intro_rel_strength(2,2,2,2,2,2) = MAX(0.5*stiffness_factor,0.4*epi_adhesion)  !Repulsive component, inter-cellular Hypoblast cortex-hypoblast cortex

      area_normalisation_factor = 0.0
      area_normalisation_count  = 0

    end subroutine scem_input

    function CROSS_PRODUCT(vector1,vector2)
      real*8, dimension(3), intent(in) :: vector1
      real*8, dimension(3), intent(in) :: vector2
      real*8, dimension(3) :: CROSS_PRODUCT

      CROSS_PRODUCT(1) = vector1(2)*vector2(3)-vector1(3)*vector2(2)
      CROSS_PRODUCT(2) = vector1(3)*vector2(1)-vector1(1)*vector2(3)
      CROSS_PRODUCT(3) = vector1(1)*vector2(2)-vector1(2)*vector2(1)
    end function CROSS_PRODUCT

end module scem_0_input
