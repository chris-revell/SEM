!C.K. Revell October 2015
!Module to control background potential for ScEM system

module scem_2_background

  use scem_0_input
  use scem_0_useful
  use scem_1_types

  implicit none

  contains

  subroutine scem_background

!    integer, intent(in) :: background
    real*8              :: cylindrical_radius  !Radius of element relative to centre of "test tube"
    real*8,dimension(2) :: normalised_2d_position !Normalised x and y coordinates of element

    real*8              :: spherical_radius    !Radius of element relative to centre of spherical background potential well
    real*8,dimension(3) :: normalised_3d_position !xyz coordinates of element normalised by spherical_radius


!    for all elements
!    work out where the element is
!    apply force to/change velocity of element according to position
!    Create a set of background potentials with numbers to specify which one to use in a given simulation?

    if (flag_background.eq.1) then !"test tube" background potential
      do n=1, ne
        cylindrical_radius = sqrt(elements(n)%position(1)**2 + &
          elements(n)%position(3)**2)
        normalised_2d_position(1) = elements(n)%position(1)/cylindrical_radius
        normalised_2d_position(2) = elements(n)%position(2)/cylindrical_radius

        if (elements(n)%position(3).LT.-60) then
          elements(n)%velocity(3) = elements(n)%velocity(3) + &   !Boundary exclusion
            100*(elements(n)%position(3)+60)**2

        elseif (cylindrical_radius.GT.300) then
          elements(n)%velocity(1) = elements(n)%velocity(1) - &   !Boundary exclusion
            normalised_2d_position(1)*(cylindrical_radius-300)**2
          elements(n)%velocity(2) = elements(n)%velocity(2) - &
            normalised_2d_position(2)*(cylindrical_radius-300)**2
          else
            elements(n)%velocity(3) = elements(n)%velocity(3) - 0.01 !Term for elements within "test tube" from gravitational force
        endif
      enddo
    elseif(flag_background.eq.2) then !Spherical potential well
      do n=1, ne
        spherical_radius = sqrt(elements(n)%position(1)**2 + &
          elements(n)%position(2)**2 + elements(n)%position(3)**2)
        normalised_3d_position(1) = elements(n)%position(1)/spherical_radius
        normalised_3d_position(2) = elements(n)%position(2)/spherical_radius
        normalised_3d_position(3) = elements(n)%position(3)/spherical_radius

        if (spherical_radius.gt.25) then
          elements(n)%velocity(:) = elements(n)%velocity(:) - &
            normalised_3d_position(:)*(spherical_radius-20)**2    !Quadratic potential beyond boundary at radius =20.
        endif
      enddo
    endif

  end subroutine

end module scem_2_background
