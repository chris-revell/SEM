! T. J Newman, Tempe, September 2010

module scem_2_diffusion

  use scem_0_arrays
  use scem_0_input
  use scem_1_types
  use omp_lib
  
  implicit none

  contains

    subroutine scem_diffusion

      integer            :: n
      real*8             :: amp,arg
      real, dimension(4) :: ra
      real, dimension(3) :: gauss

      !$omp parallel &
      !$omp shared (elements) &
      !$omp private (amp,arg,gauss,ra)
      !$omp do
      do n=1,ne
         CALL RANDOM_NUMBER(ra)      !Fill array ra with four random numbers and create three Gaussian random variates
         amp=sqrt(-2.0*log(ra(1)))
         arg=2*pi*ra(2)				     !Converts random number to an angle
         gauss(1)=amp*cos(arg)
         gauss(2)=amp*sin(arg)
         amp=sqrt(-2.0*log(ra(3)))
         arg=2*pi*ra(4)
         gauss(3)=amp*cos(arg)
         elements(n)%position(:)=elements(n)%position(:)+diff_amp*gauss(:)
      end do
      !$omp end do
      !$omp end parallel

    end subroutine scem_diffusion

end module scem_2_diffusion
