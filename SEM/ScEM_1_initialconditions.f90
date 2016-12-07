!CK Revell, May 2016
!Module to output the initial conditions of a run in the ScEM program.

module ScEM_1_initialconditions

  use scem_0_input

  implicit none

contains

  subroutine scem_initialconditions

    open(unit=22,file=output_folder//'/initial_conditions.txt',status='unknown')

    write(22,"(*(G0,:,1X))")'random number seed         = ',seed_array
    write(22,"(*(G0,:,1X))")'initial number of cells    = ',nc
    write(22,"(*(G0,:,1X))")'initial number of elements = ',ne
    write(22,"(*(G0,:,1X))")'initial number of pairs    = ',np
    write(22,"(*(G0,:,1X))")'run time                   = ',time_max
    write(22,"(*(G0,:,1X))")'frac_interaction_max       = ',frac_interaction_max
    write(22,"(*(G0,:,1X))")'elements per cell          = ',ne_cell
    write(22,"(*(G0,:,1X))") ""
    write(22,"(*(G0,:,1X))") "flag_create     = ",flag_create
    write(22,"(*(G0,:,1X))") "flag_diffusion  = ",flag_diffusion
    write(22,"(*(G0,:,1X))") "flag_conserve   = ",flag_conserve
    write(22,"(*(G0,:,1X))") "flag_background = ",flag_background
    write(22,"(*(G0,:,1X))") "flag_growth     = ",flag_growth
    write(22,"(*(G0,:,1X))") "flag_division   = ",flag_division
    write(22,"(*(G0,:,1X))") ""
    write(22,"(*(G0,:,1X))") "flag_povray     = ",flag_povray
    write(22,"(*(G0,:,1X))") "  flag_povray_volumes      = ",flag_povray_volumes
    write(22,"(*(G0,:,1X))") "  flag_povray_elements     = ",flag_povray_elements
    write(22,"(*(G0,:,1X))") "  flag_povray_pairs        = ",flag_povray_pairs
    write(22,"(*(G0,:,1X))") "  flag_povray_triangles    = ",flag_povray_triangles
    write(22,"(*(G0,:,1X))") "  flag_povray_cortex_pairs = ",flag_povray_cortex_pairs
    write(22,"(*(G0,:,1X))") "flag_count_output        = ",flag_count_output
    write(22,"(*(G0,:,1X))") "flag_fate_output         = ",flag_fate_output
    write(22,"(*(G0,:,1X))") "flag_volume_output       = ",flag_volume_output
    write(22,"(*(G0,:,1X))") "flag_elements_final      = ",flag_elements_final
    write(22,"(*(G0,:,1X))") "flag_measure_radius      = ",flag_measure_radius
    write(22,"(*(G0,:,1X))") "flag_measure_neighbours  = ",flag_measure_neighbours
    write(22,"(*(G0,:,1X))") "flag_measure_displacement= ",flag_measure_displacement
    write(22,"(*(G0,:,1X))") "flag_measure_type_radius = ",flag_measure_type_radius
    write(22,"(*(G0,:,1X))") "flag_measure_surface     = ",flag_measure_surface
    write(22,"(*(G0,:,1X))") "flag_measure_velocity    = ",flag_measure_velocity
    write(22,"(*(G0,:,1X))") "flag_measure_com         = ",flag_measure_com
    write(22,"(*(G0,:,1X))") "flag_measure_randomised  = ",flag_measure_randomised
    write(22,"(*(G0,:,1X))") ""
    write(22,"(*(G0,:,1X))") "nc_initial           = ",nc_initial
    write(22,"(*(G0,:,1X))") "stiffness_factor     = ",stiffness_factor
    write(22,"(*(G0,:,1X))") 'cell cycle time      = ',cell_cycle_time
    write(22,"(*(G0,:,1X))") 'number of cell cycles= ',n_cellcycles
    write(22,"(*(G0,:,1X))") "epi_adhesion         = ",epi_adhesion
    write(22,"(*(G0,:,1X))") "hypo_adhesion        = ",hypo_adhesion
    write(22,"(*(G0,:,1X))") "epi_hypo_adhesion    = ",epi_hypo_adhesion
    write(22,"(*(G0,:,1X))") 'cortex_constant1     = ',cortex_constant1
    write(22,"(*(G0,:,1X))") 'cortex_constant2     = ',cortex_constant2
    write(22,"(*(G0,:,1X))") ""
    write(22,"(*(G0,:,1X))") "DIT_response(1,0) = ",DIT_response(1,0)," (Epiblast external system surface DIT response factor)"
    write(22,"(*(G0,:,1X))") "DIT_response(1,1) = ",DIT_response(1,1)," (Epiblast homotypic interface DIT response factor)"
    write(22,"(*(G0,:,1X))") "DIT_response(1,2) = ",DIT_response(1,2)," (Epiblast heterotypic interface DIT response factor)"
    write(22,"(*(G0,:,1X))") "DIT_response(2,0) = ",DIT_response(2,0)," (Primitive endoderm external system surface DIT response factor)"
    write(22,"(*(G0,:,1X))") "DIT_response(2,1) = ",DIT_response(2,1)," (Primitive endoderm homotypic interface DIT response factor)"
    write(22,"(*(G0,:,1X))") "DIT_response(2,2) = ",DIT_response(2,2)," (Primitive endoderm heterotypic interface DIT response factor)"
    write(22,"(*(G0,:,1X))") ""
    write(22,"(*(G0,:,1X))") "Nearest neighour interaction magnitudes"
    write(22,"(*(G0,:,1X))") "rel_strength(1,1,1,1,1,1) = ",rel_strength(1,1,1,1,1,1)," (Adhesive component, intra-cellular Epiblast cytoplasm-epiblast cytoplasm)"
    write(22,"(*(G0,:,1X))") "rel_strength(1,1,1,1,2,1) = ",rel_strength(1,1,1,1,2,1)," (Adhesive component, intra-cellular Epiblast cytoplasm-epiblast cortex)"
    write(22,"(*(G0,:,1X))") "rel_strength(1,1,1,2,2,1) = ",rel_strength(1,1,1,2,2,1)," (Adhesive component, intra-cellular Epiblast cortex-epiblast cortex)"
    write(22,"(*(G0,:,1X))") "rel_strength(1,1,2,1,1,1) = ",rel_strength(1,1,2,1,1,1)," (Adhesive component, intra-cellular Epiblast cytoplasm-hypoblast cytoplasm)"
    write(22,"(*(G0,:,1X))") "rel_strength(1,1,2,1,2,1) = ",rel_strength(1,1,2,1,2,1)," (Adhesive component, intra-cellular Epiblast cytoplasm-hypoblast cortex)"
    write(22,"(*(G0,:,1X))") "rel_strength(1,1,2,2,2,1) = ",rel_strength(1,1,2,2,2,1)," (Adhesive component, intra-cellular Epiblast cortex-hypoblast cortex)"
    write(22,"(*(G0,:,1X))") "rel_strength(1,2,2,1,1,1) = ",rel_strength(1,2,2,1,1,1)," (Adhesive component, intra-cellular Hypoblast cytoplasm-hypoblast cytoplasm)"
    write(22,"(*(G0,:,1X))") "rel_strength(1,2,2,1,2,1) = ",rel_strength(1,2,2,1,2,1)," (Adhesive component, intra-cellular Hypoblast cytoplasm-hypoblast cortex)"
    write(22,"(*(G0,:,1X))") "rel_strength(1,2,2,2,2,1) = ",rel_strength(1,2,2,2,2,1)," (Adhesive component, intra-cellular Hypoblast cortex-hypoblast cortex)"

    write(22,"(*(G0,:,1X))") "rel_strength(1,1,1,1,1,2) = ",rel_strength(1,1,1,1,1,2)," (Adhesive component, inter-cellular Epiblast cytoplasm-epiblast cytoplasm)"
    write(22,"(*(G0,:,1X))") "rel_strength(1,1,1,1,2,2) = ",rel_strength(1,1,1,1,2,2)," (Adhesive component, inter-cellular Epiblast cytoplasm-epiblast cortex)"
    write(22,"(*(G0,:,1X))") "rel_strength(1,1,1,2,2,2) = ",rel_strength(1,1,1,2,2,2)," (Adhesive component, inter-cellular Epiblast cortex-epiblast cortex)"
    write(22,"(*(G0,:,1X))") "rel_strength(1,1,2,1,1,2) = ",rel_strength(1,1,2,1,1,2)," (Adhesive component, inter-cellular Epiblast cytoplasm-hypoblast cytoplasm)"
    write(22,"(*(G0,:,1X))") "rel_strength(1,1,2,1,2,2) = ",rel_strength(1,1,2,1,2,2)," (Adhesive component, inter-cellular Epiblast cytoplasm-hypoblast cortex)"
    write(22,"(*(G0,:,1X))") "rel_strength(1,1,2,2,2,2) = ",rel_strength(1,1,2,2,2,2)," (Adhesive component, inter-cellular Epiblast cortex-hypoblast cortex)"
    write(22,"(*(G0,:,1X))") "rel_strength(1,2,2,1,1,2) = ",rel_strength(1,2,2,1,1,2)," (Adhesive component, inter-cellular Hypoblast cytoplasm-hypoblast cytoplasm)"
    write(22,"(*(G0,:,1X))") "rel_strength(1,2,2,1,2,2) = ",rel_strength(1,2,2,1,2,2)," (Adhesive component, inter-cellular Hypoblast cytoplasm-hypoblast cortex)"
    write(22,"(*(G0,:,1X))") "rel_strength(1,2,2,2,2,2) = ",rel_strength(1,2,2,2,2,2)," (Adhesive component, inter-cellular Hypoblast cortex-hypoblast cortex)"

    write(22,"(*(G0,:,1X))") "rel_strength(2,1,1,1,1,1) = ",rel_strength(2,1,1,1,1,1)," (Repulsive component, intra-cellular Epiblast cytoplasm-epiblast cytoplasm)"
    write(22,"(*(G0,:,1X))") "rel_strength(2,1,1,1,2,1) = ",rel_strength(2,1,1,1,2,1)," (Repulsive component, intra-cellular Epiblast cytoplasm-epiblast cortex)"
    write(22,"(*(G0,:,1X))") "rel_strength(2,1,1,2,2,1) = ",rel_strength(2,1,1,2,2,1)," (Repulsive component, intra-cellular Epiblast cortex-epiblast cortex)"
    write(22,"(*(G0,:,1X))") "rel_strength(2,1,2,1,1,1) = ",rel_strength(2,1,2,1,1,1)," (Repulsive component, intra-cellular Epiblast cytoplasm-hypoblast cytoplasm)"
    write(22,"(*(G0,:,1X))") "rel_strength(2,1,2,1,2,1) = ",rel_strength(2,1,2,1,2,1)," (Repulsive component, intra-cellular Epiblast cytoplasm-hypoblast cortex)"
    write(22,"(*(G0,:,1X))") "rel_strength(2,1,2,2,2,1) = ",rel_strength(2,1,2,2,2,1)," (Repulsive component, intra-cellular Epiblast cortex-hypoblast cortex)"
    write(22,"(*(G0,:,1X))") "rel_strength(2,2,2,1,1,1) = ",rel_strength(2,2,2,1,1,1)," (Repulsive component, intra-cellular Hypoblast cytoplasm-hypoblast cytoplasm)"
    write(22,"(*(G0,:,1X))") "rel_strength(2,2,2,1,2,1) = ",rel_strength(2,2,2,1,2,1)," (Repulsive component, intra-cellular Hypoblast cytoplasm-hypoblast cortex)"
    write(22,"(*(G0,:,1X))") "rel_strength(2,2,2,2,2,1) = ",rel_strength(2,2,2,2,2,1)," (Repulsive component, intra-cellular Hypoblast cortex-hypoblast cortex)"

    write(22,"(*(G0,:,1X))") "rel_strength(2,1,1,1,1,2) = ",rel_strength(2,1,1,1,1,2)," (Repulsive component, inter-cellular Epiblast cytoplasm-epiblast cytoplasm)"
    write(22,"(*(G0,:,1X))") "rel_strength(2,1,1,1,2,2) = ",rel_strength(2,1,1,1,2,2)," (Repulsive component, inter-cellular Epiblast cytoplasm-epiblast cortex)"
    write(22,"(*(G0,:,1X))") "rel_strength(2,1,1,2,2,2) = ",rel_strength(2,1,1,2,2,2)," (Repulsive component, inter-cellular Epiblast cortex-epiblast cortex)"
    write(22,"(*(G0,:,1X))") "rel_strength(2,1,2,1,1,2) = ",rel_strength(2,1,2,1,1,2)," (Repulsive component, inter-cellular Epiblast cytoplasm-hypoblast cytoplasm)"
    write(22,"(*(G0,:,1X))") "rel_strength(2,1,2,1,2,2) = ",rel_strength(2,1,2,1,2,2)," (Repulsive component, inter-cellular Epiblast cytoplasm-hypoblast cortex)"
    write(22,"(*(G0,:,1X))") "rel_strength(2,1,2,2,2,2) = ",rel_strength(2,1,2,2,2,2)," (Repulsive component, inter-cellular Epiblast cortex-hypoblast cortex)"
    write(22,"(*(G0,:,1X))") "rel_strength(2,2,2,1,1,2) = ",rel_strength(2,2,2,1,1,2)," (Repulsive component, inter-cellular Hypoblast cytoplasm-hypoblast cytoplasm)"
    write(22,"(*(G0,:,1X))") "rel_strength(2,2,2,1,2,2) = ",rel_strength(2,2,2,1,2,2)," (Repulsive component, inter-cellular Hypoblast cytoplasm-hypoblast cortex)"
    write(22,"(*(G0,:,1X))") "rel_strength(2,2,2,2,2,2) = ",rel_strength(2,2,2,2,2,2)," (Repulsive component, inter-cellular Hypoblast cortex-hypoblast cortex)"

    close(unit=22)

  end subroutine scem_initialconditions
end module ScEM_1_initialconditions
