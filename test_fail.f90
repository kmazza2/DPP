program test_fail

   use, intrinsic :: iso_fortran_env
   implicit none

   real(real64) :: x
   x = 25.0_real64
   print *, x
   stop 'Fail for GitHub Actions'

end program test_fail
