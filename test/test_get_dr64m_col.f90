program test_get_dr64m_col

   use, intrinsic :: iso_fortran_env
   use :: matrix

   implicit none

   type(dr64m) :: test_matrix
   type(dr64m) :: test_column
   real(real64), dimension(2,1) :: expected

   test_matrix = new_dr64m(reshape(source = [1.0_real64, 3.0_real64, 2.0_real64, 4.0_real64], shape = [2, 2]))
   test_column = get_dr64m_col(test_matrix, 2)
   expected = reshape(source = [2.0_real64, 4.0_real64], shape = [2, 1])
   if (.not. all(test_column%array == expected)) then
      error stop 'Column contains unexpected value.'
   end if
end program test_get_dr64m_col
