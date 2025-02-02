program test_leak

   use, intrinsic :: iso_fortran_env
   use :: matrix

   implicit none

   type(dr64m) :: matrix1, matrix2, matrix3
   integer(int32) :: i


   matrix1 = new_dr64m(reshape(source = [ &
           1.0_real64, 4.0_real64, 2.0_real64, 5.0_real64, 3.0_real64, 6.0_real64 &
           ], shape = [2, 3]))
   matrix2 = new_dr64m(reshape(source = [ 2.0_real64, 5.0_real64, 3.0_real64, &
           6.0_real64, 4.0_real64, 7.0_real64], shape = [2, 3]))
   i = 1
   do
      print *, i
      if (i < 0) then
         stop 'Silent integer overflow.'
      end if
      matrix3 = attempt_leak(matrix1, matrix2, i)
      i = i + 1
   end do

   contains

   function attempt_leak(m1, m2, i)
      use :: matrix
      type(dr64m), intent(in) :: m1, m2
      integer(int32), intent(in) :: i
      type(dr64m) :: attempt_leak
      type(dr64m) :: m3
      m3 = m1 + m2
      m3%array(1,1) = m3%array(1,1) + i
      attempt_leak = new_dr64m(m3%array)
   end function attempt_leak

end program test_leak
