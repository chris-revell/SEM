module cube_root_module

  implicit none

contains

  real*8 function cube_root(x)

    real*8,intent(inout)  :: x
    real*8                :: log_x

    ! Calculate cube root by using logs
    log_x = log(x)
    cube_root = exp(log_x/3.0)

    return

  end function cube_root

end module cube_root_module
