program test_get_dr64m_row

   use, intrinsic :: iso_fortran_env
   use :: matrix
   use :: vector_spaces

   implicit none

    type(dr64m) :: b_2_4
    type(dr64m) :: b_1_1
    type(dr64m) :: b_3_3

    b_2_4 = standard_basis(2_int32, 4_int32)
    b_1_1 = standard_basis(1_int32, 1_int32)
    b_3_3 = standard_basis(3_int32, 3_int32)
    if ( &
          .not. ( &
             all(b_2_4%array == reshape(source = [0.0_real64, 1.0_real64, 0.0_real64, 0.0_real64], shape = [4, 1])) .and. &
             all(b_1_1%array == reshape(source = [1.0_real64], shape = [1, 1])) .and. &
             all(b_3_3%array == reshape(source = [0.0_real64, 0.0_real64, 1.0_real64], shape = [3, 1])) &
          ) &
    ) then
       error stop 'Did not generate expected basis vectors.'
    end if
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
