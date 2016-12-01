! T. J Newman, Tempe, September 2010

! this module created from submodule of scem_2_growth, February 2011

module scem_2_ageing

  use scem_0_arrays
  use scem_0_input
  use scem_1_types
  use omp_lib

  implicit none

  contains

    subroutine scem_ageing

      integer :: n

      ! update age and stage of each element
      !$omp parallel &
      !$omp shared (ne,elements,dt,establishment_time) &
      !$omp private (n)
      !$omp do
      do n=1,ne
         elements(n)%age=elements(n)%age+dt
         if (elements(n)%stage.eq.0) then
            ! elements(n)%strength=elements(n)%age/establishment_time
            elements(n)%strength=(elements(n)%age/establishment_time)**2
            if (elements(n)%age.gt.establishment_time) then
               elements(n)%stage=1
               elements(n)%strength=1.0
            end if
         endif
      end do
      !$omp end do
      !$omp end parallel

      ! update age of each cell
      do n=1,nc
         cells(n)%age=cells(n)%age+dt
      end do

    end subroutine scem_ageing

end module scem_2_ageing
