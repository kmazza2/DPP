program test_get_dr64m_row

   use, intrinsic :: iso_fortran_env
   use :: matrix
   use :: vector_spaces

   implicit none

!   type(dr64m) :: test_matrix
!   type(dr64m) :: test_row
!   real(real64), dimension(1,2) :: expected
!
!   test_matrix = new_dr64m(reshape(source = [1.0_real64, 3.0_real64, 2.0_real64, 4.0_real64], shape = [2, 2]))
!   test_row = get_dr64m_row(test_matrix, 2)
!   expected = reshape(source = [3.0_real64, 4.0_real64], shape = [1, 2])
!   if (.not. all(test_row%array == expected)) then
!      error stop 'Row contains unexpected value.'
!   end if
end program test_get_dr64m_row
