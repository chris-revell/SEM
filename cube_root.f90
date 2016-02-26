function cube_root(x)

  implicit none

!  function cube_root(x) !result(root)

  real,intent(in)  :: x
  real,intent(out) :: cube_root
  real             :: log_x

  ! Calculate cube root by using logs
  log_x = log(x)

  cube_root = exp(log_x/3.0)

end function cube_root
