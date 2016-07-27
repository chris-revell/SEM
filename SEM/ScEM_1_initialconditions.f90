!CK Revell, May 2016
!Module to output the initial conditions of a run in the ScEM program.

module ScEM_1_initialconditions

  use scem_0_input

  implicit none

contains

  subroutine scem_initialconditions

    open(unit=22,file=output_folder//'/system_data/initial_conditions.txt',status='unknown')

    write(22,"(*(G0,:,1X))")'random number seed         = ',seed_array
    write(22,"(*(G0,:,1X))")'initial number of cells    = ',nc
    write(22,"(*(G0,:,1X))")'initial number of elements = ',ne
    write(22,"(*(G0,:,1X))")'initial number of pairs    = ',np
    write(22,"(*(G0,:,1X))")'run time                   = ',time_max
    write(22,"(*(G0,:,1X))")'cell cycle time            = ',cell_cycle_time
    write(22,"(*(G0,:,1X))")'number of cell cycles      = ',time_max/cell_cycle_time
    write(22,"(*(G0,:,1X))")'frac_interaction_max       = ',frac_interaction_max
    write(22,"(*(G0,:,1X))")'ne_cell                    = ',ne_cell

    write(22,"(*(G0,:,1X))")'cortex_constant = ',cortex_constant


    write(22,"(*(G0,:,1X))") "flag_create     = ",flag_create
    write(22,"(*(G0,:,1X))") "flag_diffusion  = ",flag_diffusion
    write(22,"(*(G0,:,1X))") "flag_conserve   = ",flag_conserve
    write(22,"(*(G0,:,1X))") "flag_background = ",flag_background
    write(22,"(*(G0,:,1X))") "flag_growth     = ",flag_growth
    write(22,"(*(G0,:,1X))") "flag_division   = ",flag_division
    write(22,"(*(G0,:,1X))") "flag_cortex     = ",flag_cortex
    write(22,"(*(G0,:,1X))") "flag_DIT        = ",flag_DIT

    write(22,"(*(G0,:,1X))") "flag_povray     = ",flag_povray
    write(22,"(*(G0,:,1X))") "  flag_povray_volumes      = ",flag_povray_volumes
    write(22,"(*(G0,:,1X))") "  flag_povray_elements     = ",flag_povray_elements
    write(22,"(*(G0,:,1X))") "  flag_povray_pairs        = ",flag_povray_pairs
    write(22,"(*(G0,:,1X))") "  flag_povray_triangles    = ",flag_povray_triangles
    write(22,"(*(G0,:,1X))") "  flag_povray_cortex_pairs = ",flag_povray_cortex_pairs
    write(22,"(*(G0,:,1X))") "flag_count_output       = ",flag_count_output
    write(22,"(*(G0,:,1X))") "flag_fate_output        = ",flag_fate_output
    write(22,"(*(G0,:,1X))") "flag_volume_output      = ",flag_volume_output
    write(22,"(*(G0,:,1X))") "flag_elements_final     = ",flag_elements_final
    write(22,"(*(G0,:,1X))") "flag_measure_interface            = ",flag_measure_interface
    write(22,"(*(G0,:,1X))") "flag_measure_radius     = ",flag_measure_radius
    write(22,"(*(G0,:,1X))") "flag_measure_neighbours = ",flag_measure_neighbours

    write(22,"(*(G0,:,1X))") "Nearest neighour interaction magnitudes"
    write(22,"(*(G0,:,1X))") "rel_strength(1,1,1,1,1,1) = ",rel_strength(1,1,1,1,1,1),"(Adhesive component, intra-cellular Epiblast cytoplasm-epiblast cytoplasm)"
    write(22,"(*(G0,:,1X))") "rel_strength(1,1,1,1,2,1) = ",rel_strength(1,1,1,1,2,1),"(Adhesive component, intra-cellular Epiblast cytoplasm-epiblast cortex)"
    write(22,"(*(G0,:,1X))") "rel_strength(1,1,1,2,2,1) = ",rel_strength(1,1,1,2,2,1),"(Adhesive component, intra-cellular Epiblast cortex-epiblast cortex)"
    write(22,"(*(G0,:,1X))") "rel_strength(1,1,2,1,1,1) = ",rel_strength(1,1,2,1,1,1),"(Adhesive component, intra-cellular Epiblast cytoplasm-hypoblast cytoplasm)"
    write(22,"(*(G0,:,1X))") "rel_strength(1,1,2,1,2,1) = ",rel_strength(1,1,2,1,2,1),"(Adhesive component, intra-cellular Epiblast cytoplasm-hypoblast cortex)"
    write(22,"(*(G0,:,1X))") "rel_strength(1,1,2,2,2,1) = ",rel_strength(1,1,2,2,2,1),"(Adhesive component, intra-cellular Epiblast cortex-hypoblast cortex)"
    write(22,"(*(G0,:,1X))") "rel_strength(1,2,2,1,1,1) = ",rel_strength(1,2,2,1,1,1),"(Adhesive component, intra-cellular Hypoblast cytoplasm-hypoblast cytoplasm)"
    write(22,"(*(G0,:,1X))") "rel_strength(1,2,2,1,2,1) = ",rel_strength(1,2,2,1,2,1),"(Adhesive component, intra-cellular Hypoblast cytoplasm-hypoblast cortex)"
    write(22,"(*(G0,:,1X))") "rel_strength(1,2,2,2,2,1) = ",rel_strength(1,2,2,2,2,1),"(Adhesive component, intra-cellular Hypoblast cortex-hypoblast cortex)"

    write(22,"(*(G0,:,1X))") "rel_strength(1,1,1,1,1,2) = ",rel_strength(1,1,1,1,1,2),"(Adhesive component, inter-cellular Epiblast cytoplasm-epiblast cytoplasm)"
    write(22,"(*(G0,:,1X))") "rel_strength(1,1,1,1,2,2) = ",rel_strength(1,1,1,1,2,2),"(Adhesive component, inter-cellular Epiblast cytoplasm-epiblast cortex)"
    write(22,"(*(G0,:,1X))") "rel_strength(1,1,1,2,2,2) = ",rel_strength(1,1,1,2,2,2),"(Adhesive component, inter-cellular Epiblast cortex-epiblast cortex)"
    write(22,"(*(G0,:,1X))") "rel_strength(1,1,2,1,1,2) = ",rel_strength(1,1,2,1,1,2),"(Adhesive component, inter-cellular Epiblast cytoplasm-hypoblast cytoplasm)"
    write(22,"(*(G0,:,1X))") "rel_strength(1,1,2,1,2,2) = ",rel_strength(1,1,2,1,2,2),"(Adhesive component, inter-cellular Epiblast cytoplasm-hypoblast cortex)"
    write(22,"(*(G0,:,1X))") "rel_strength(1,1,2,2,2,2) = ",rel_strength(1,1,2,2,2,2),"(Adhesive component, inter-cellular Epiblast cortex-hypoblast cortex)"
    write(22,"(*(G0,:,1X))") "rel_strength(1,2,2,1,1,2) = ",rel_strength(1,2,2,1,1,2),"(Adhesive component, inter-cellular Hypoblast cytoplasm-hypoblast cytoplasm)"
    write(22,"(*(G0,:,1X))") "rel_strength(1,2,2,1,2,2) = ",rel_strength(1,2,2,1,2,2),"(Adhesive component, inter-cellular Hypoblast cytoplasm-hypoblast cortex)"
    write(22,"(*(G0,:,1X))") "rel_strength(1,2,2,2,2,2) = ",rel_strength(1,2,2,2,2,2),"(Adhesive component, inter-cellular Hypoblast cortex-hypoblast cortex)"

    write(22,"(*(G0,:,1X))") "rel_strength(2,1,1,1,1,1) = ",rel_strength(2,1,1,1,1,1),"(Repulsive component, intra-cellular Epiblast cytoplasm-epiblast cytoplasm)"
    write(22,"(*(G0,:,1X))") "rel_strength(2,1,1,1,2,1) = ",rel_strength(2,1,1,1,2,1),"(Repulsive component, intra-cellular Epiblast cytoplasm-epiblast cortex)"
    write(22,"(*(G0,:,1X))") "rel_strength(2,1,1,2,2,1) = ",rel_strength(2,1,1,2,2,1),"(Repulsive component, intra-cellular Epiblast cortex-epiblast cortex)"
    write(22,"(*(G0,:,1X))") "rel_strength(2,1,2,1,1,1) = ",rel_strength(2,1,2,1,1,1),"(Repulsive component, intra-cellular Epiblast cytoplasm-hypoblast cytoplasm)"
    write(22,"(*(G0,:,1X))") "rel_strength(2,1,2,1,2,1) = ",rel_strength(2,1,2,1,2,1),"(Repulsive component, intra-cellular Epiblast cytoplasm-hypoblast cortex)"
    write(22,"(*(G0,:,1X))") "rel_strength(2,1,2,2,2,1) = ",rel_strength(2,1,2,2,2,1),"(Repulsive component, intra-cellular Epiblast cortex-hypoblast cortex)"
    write(22,"(*(G0,:,1X))") "rel_strength(2,2,2,1,1,1) = ",rel_strength(2,2,2,1,1,1),"(Repulsive component, intra-cellular Hypoblast cytoplasm-hypoblast cytoplasm)"
    write(22,"(*(G0,:,1X))") "rel_strength(2,2,2,1,2,1) = ",rel_strength(2,2,2,1,2,1),"(Repulsive component, intra-cellular Hypoblast cytoplasm-hypoblast cortex)"
    write(22,"(*(G0,:,1X))") "rel_strength(2,2,2,2,2,1) = ",rel_strength(2,2,2,2,2,1),"(Repulsive component, intra-cellular Hypoblast cortex-hypoblast cortex)"

    write(22,"(*(G0,:,1X))") "rel_strength(2,1,1,1,1,2) = ",rel_strength(2,1,1,1,1,2),"(Repulsive component, inter-cellular Epiblast cytoplasm-epiblast cytoplasm)"
    write(22,"(*(G0,:,1X))") "rel_strength(2,1,1,1,2,2) = ",rel_strength(2,1,1,1,2,2),"(Repulsive component, inter-cellular Epiblast cytoplasm-epiblast cortex)"
    write(22,"(*(G0,:,1X))") "rel_strength(2,1,1,2,2,2) = ",rel_strength(2,1,1,2,2,2),"(Repulsive component, inter-cellular Epiblast cortex-epiblast cortex)"
    write(22,"(*(G0,:,1X))") "rel_strength(2,1,2,1,1,2) = ",rel_strength(2,1,2,1,1,2),"(Repulsive component, inter-cellular Epiblast cytoplasm-hypoblast cytoplasm)"
    write(22,"(*(G0,:,1X))") "rel_strength(2,1,2,1,2,2) = ",rel_strength(2,1,2,1,2,2),"(Repulsive component, inter-cellular Epiblast cytoplasm-hypoblast cortex)"
    write(22,"(*(G0,:,1X))") "rel_strength(2,1,2,2,2,2) = ",rel_strength(2,1,2,2,2,2),"(Repulsive component, inter-cellular Epiblast cortex-hypoblast cortex)"
    write(22,"(*(G0,:,1X))") "rel_strength(2,2,2,1,1,2) = ",rel_strength(2,2,2,1,1,2),"(Repulsive component, inter-cellular Hypoblast cytoplasm-hypoblast cytoplasm)"
    write(22,"(*(G0,:,1X))") "rel_strength(2,2,2,1,2,2) = ",rel_strength(2,2,2,1,2,2),"(Repulsive component, inter-cellular Hypoblast cytoplasm-hypoblast cortex)"
    write(22,"(*(G0,:,1X))") "rel_strength(2,2,2,2,2,2) = ",rel_strength(2,2,2,2,2,2),"(Repulsive component, inter-cellular Hypoblast cortex-hypoblast cortex)"

    !write(22,"(*(G0,:,1X))") "rel_strength(adhesive 1/repulsive 2,fate1,fate2,type1,type1,intra 1/inter 2)"
    !do i=1,2
    !  do j=1,2
    !    do k=1,2
    !      do l=1,2
    !        do m=1,2
    !          do n=1,2
    !            write(22,"(*(G0,:,1X))") "rel_strength("i,j,k,l,m,n,") = ",rel_strength(i,j,k,l,m,n)
    !          enddo
    !        enddo
    !      enddo
    !    enddo
    !  enddo
    !enddo

    close(unit=22)

  end subroutine scem_initialconditions
end module ScEM_1_initialconditions
