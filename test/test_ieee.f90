program test_ieee

   use, intrinsic :: ieee_arithmetic
   use, intrinsic :: ieee_features
   use, intrinsic :: iso_fortran_env

   implicit none


   logical :: int32_supported
   logical :: int64_supported
   logical :: real64_supported
   logical :: real128_supported

   logical :: int_is_int32
   logical :: dp_is_real64

   logical :: real64_overflow_supported
   logical :: real64_underflow_supported
   logical :: real64_divide_by_zero_supported
   logical :: real64_inexact_supported
   logical :: real64_invalid_supported

   logical :: real128_overflow_supported
   logical :: real128_underflow_supported
   logical :: real128_divide_by_zero_supported
   logical :: real128_inexact_supported
   logical :: real128_invalid_supported

   logical :: real64_ieee_standard_supported
   logical :: real64_underflow_control_supported

   logical :: real128_ieee_standard_supported
   logical :: real128_underflow_control_supported

   integer :: i
   real(real64) :: x_real64
   real(real128) :: x_real128

   logical :: overflow_flag_set
   logical :: underflow_flag_set
   logical :: divide_by_zero_flag_set
   logical :: inexact_flag_set
   logical :: invalid_flag_set

   logical :: real64_overflow_detected
   logical :: real64_underflow_detected
   logical :: real64_divide_by_zero_detected
   logical :: real64_invalid_detected

   logical :: real128_overflow_detected
   logical :: real128_underflow_detected
   logical :: real128_divide_by_zero_detected
   logical :: real128_invalid_detected

   logical :: log_negative_detected

   int32_supported = .not. ((int32 == -2) .or. (int32 == -1))
   int64_supported = .not. ((int64 == -2) .or. (int64 == -1))
   real64_supported = .not. ((real64 == -2) .or. (real64 == -1))
   real128_supported = .not. ((real128 == -2) .or. (real128 == -1))
   int_is_int32 = kind(i) == int32
   dp_is_real64 = kind(1d0) == real64

   real64_overflow_supported = ieee_support_flag(ieee_overflow, x_real64)
   real64_underflow_supported = ieee_support_flag(ieee_underflow, x_real64)
   real64_divide_by_zero_supported = ieee_support_flag(ieee_divide_by_zero, x_real64)
   real64_inexact_supported = ieee_support_flag(ieee_inexact, x_real64)
   real64_invalid_supported = ieee_support_flag(ieee_invalid, x_real64)

   real128_overflow_supported = ieee_support_flag(ieee_overflow, x_real128)
   real128_underflow_supported = ieee_support_flag(ieee_underflow, x_real128)
   real128_divide_by_zero_supported = ieee_support_flag(ieee_divide_by_zero, x_real128)
   real128_inexact_supported = ieee_support_flag(ieee_inexact, x_real128)
   real128_invalid_supported = ieee_support_flag(ieee_invalid, x_real128)

   real64_ieee_standard_supported = ieee_support_standard(x_real64)
   real64_underflow_control_supported = ieee_support_underflow_control(x_real64)

   real128_ieee_standard_supported = ieee_support_standard(x_real128)
   real128_underflow_control_supported = ieee_support_underflow_control(x_real128)





   call ieee_set_flag(ieee_overflow, .false.)
   call ieee_set_flag(ieee_underflow, .false.)
   call ieee_set_flag(ieee_divide_by_zero, .false.)
   call ieee_set_flag(ieee_inexact, .false.)
   call ieee_set_flag(ieee_invalid, .false.)

   call ieee_get_flag(ieee_overflow, overflow_flag_set)
   call ieee_get_flag(ieee_underflow, underflow_flag_set)
   call ieee_get_flag(ieee_divide_by_zero, divide_by_zero_flag_set)
   call ieee_get_flag(ieee_inexact, inexact_flag_set)
   call ieee_get_flag(ieee_invalid, invalid_flag_set)
   if ( &
           overflow_flag_set .or. &
           underflow_flag_set .or. &
           divide_by_zero_flag_set .or. &
           inexact_flag_set .or. &
           invalid_flag_set &
   ) then
      error stop 'Failed to reset IEEE exception flag.'
   end if
   call test_real64_ieee_overflow
   call ieee_get_flag(ieee_overflow, overflow_flag_set)
   call ieee_get_flag(ieee_underflow, underflow_flag_set)
   call ieee_get_flag(ieee_divide_by_zero, divide_by_zero_flag_set)
   call ieee_get_flag(ieee_inexact, inexact_flag_set)
   call ieee_get_flag(ieee_invalid, invalid_flag_set)
   real64_overflow_detected = overflow_flag_set
   call ieee_set_flag(ieee_overflow, .false.)
   call ieee_set_flag(ieee_underflow, .false.)
   call ieee_set_flag(ieee_divide_by_zero, .false.)
   call ieee_set_flag(ieee_inexact, .false.)
   call ieee_set_flag(ieee_invalid, .false.)

   call ieee_get_flag(ieee_overflow, overflow_flag_set)
   call ieee_get_flag(ieee_underflow, underflow_flag_set)
   call ieee_get_flag(ieee_divide_by_zero, divide_by_zero_flag_set)
   call ieee_get_flag(ieee_inexact, inexact_flag_set)
   call ieee_get_flag(ieee_invalid, invalid_flag_set)
   if ( &
           overflow_flag_set .or. &
           underflow_flag_set .or. &
           divide_by_zero_flag_set .or. &
           inexact_flag_set .or. &
           invalid_flag_set &
   ) then
      error stop 'Failed to reset IEEE exception flag.'
   end if
   call test_real64_ieee_underflow
   call ieee_get_flag(ieee_overflow, overflow_flag_set)
   call ieee_get_flag(ieee_underflow, underflow_flag_set)
   call ieee_get_flag(ieee_divide_by_zero, divide_by_zero_flag_set)
   call ieee_get_flag(ieee_inexact, inexact_flag_set)
   call ieee_get_flag(ieee_invalid, invalid_flag_set)
   real64_underflow_detected = underflow_flag_set
   call ieee_set_flag(ieee_overflow, .false.)
   call ieee_set_flag(ieee_underflow, .false.)
   call ieee_set_flag(ieee_divide_by_zero, .false.)
   call ieee_set_flag(ieee_inexact, .false.)
   call ieee_set_flag(ieee_invalid, .false.)

   call ieee_get_flag(ieee_overflow, overflow_flag_set)
   call ieee_get_flag(ieee_underflow, underflow_flag_set)
   call ieee_get_flag(ieee_divide_by_zero, divide_by_zero_flag_set)
   call ieee_get_flag(ieee_inexact, inexact_flag_set)
   call ieee_get_flag(ieee_invalid, invalid_flag_set)
   if ( &
           overflow_flag_set .or. &
           underflow_flag_set .or. &
           divide_by_zero_flag_set .or. &
           inexact_flag_set .or. &
           invalid_flag_set &
   ) then
      error stop 'Failed to reset IEEE exception flag.'
   end if
   call test_real64_ieee_divide_by_zero
   call ieee_get_flag(ieee_overflow, overflow_flag_set)
   call ieee_get_flag(ieee_underflow, underflow_flag_set)
   call ieee_get_flag(ieee_divide_by_zero, divide_by_zero_flag_set)
   call ieee_get_flag(ieee_inexact, inexact_flag_set)
   call ieee_get_flag(ieee_invalid, invalid_flag_set)
   real64_divide_by_zero_detected = divide_by_zero_flag_set
   call ieee_set_flag(ieee_overflow, .false.)
   call ieee_set_flag(ieee_underflow, .false.)
   call ieee_set_flag(ieee_divide_by_zero, .false.)
   call ieee_set_flag(ieee_inexact, .false.)
   call ieee_set_flag(ieee_invalid, .false.)

   call ieee_get_flag(ieee_overflow, overflow_flag_set)
   call ieee_get_flag(ieee_underflow, underflow_flag_set)
   call ieee_get_flag(ieee_divide_by_zero, divide_by_zero_flag_set)
   call ieee_get_flag(ieee_inexact, inexact_flag_set)
   call ieee_get_flag(ieee_invalid, invalid_flag_set)
   if ( &
           overflow_flag_set .or. &
           underflow_flag_set .or. &
           divide_by_zero_flag_set .or. &
           inexact_flag_set .or. &
           invalid_flag_set &
   ) then
      error stop 'Failed to reset IEEE exception flag.'
   end if
   call test_real64_ieee_invalid
   call ieee_get_flag(ieee_overflow, overflow_flag_set)
   call ieee_get_flag(ieee_underflow, underflow_flag_set)
   call ieee_get_flag(ieee_divide_by_zero, divide_by_zero_flag_set)
   call ieee_get_flag(ieee_inexact, inexact_flag_set)
   call ieee_get_flag(ieee_invalid, invalid_flag_set)
   real64_invalid_detected = invalid_flag_set
   call ieee_set_flag(ieee_overflow, .false.)
   call ieee_set_flag(ieee_underflow, .false.)
   call ieee_set_flag(ieee_divide_by_zero, .false.)
   call ieee_set_flag(ieee_inexact, .false.)
   call ieee_set_flag(ieee_invalid, .false.)

   call ieee_get_flag(ieee_overflow, overflow_flag_set)
   call ieee_get_flag(ieee_underflow, underflow_flag_set)
   call ieee_get_flag(ieee_divide_by_zero, divide_by_zero_flag_set)
   call ieee_get_flag(ieee_inexact, inexact_flag_set)
   call ieee_get_flag(ieee_invalid, invalid_flag_set)
   if ( &
           overflow_flag_set .or. &
           underflow_flag_set .or. &
           divide_by_zero_flag_set .or. &
           inexact_flag_set .or. &
           invalid_flag_set &
   ) then
      error stop 'Failed to reset IEEE exception flag.'
   end if
   call test_real128_ieee_overflow
   call ieee_get_flag(ieee_overflow, overflow_flag_set)
   call ieee_get_flag(ieee_underflow, underflow_flag_set)
   call ieee_get_flag(ieee_divide_by_zero, divide_by_zero_flag_set)
   call ieee_get_flag(ieee_inexact, inexact_flag_set)
   call ieee_get_flag(ieee_invalid, invalid_flag_set)
   real128_overflow_detected = overflow_flag_set
   call ieee_set_flag(ieee_overflow, .false.)
   call ieee_set_flag(ieee_underflow, .false.)
   call ieee_set_flag(ieee_divide_by_zero, .false.)
   call ieee_set_flag(ieee_inexact, .false.)
   call ieee_set_flag(ieee_invalid, .false.)

   call ieee_get_flag(ieee_overflow, overflow_flag_set)
   call ieee_get_flag(ieee_underflow, underflow_flag_set)
   call ieee_get_flag(ieee_divide_by_zero, divide_by_zero_flag_set)
   call ieee_get_flag(ieee_inexact, inexact_flag_set)
   call ieee_get_flag(ieee_invalid, invalid_flag_set)
   if ( &
           overflow_flag_set .or. &
           underflow_flag_set .or. &
           divide_by_zero_flag_set .or. &
           inexact_flag_set .or. &
           invalid_flag_set &
   ) then
      error stop 'Failed to reset IEEE exception flag.'
   end if
   call test_real128_ieee_underflow
   call ieee_get_flag(ieee_overflow, overflow_flag_set)
   call ieee_get_flag(ieee_underflow, underflow_flag_set)
   call ieee_get_flag(ieee_divide_by_zero, divide_by_zero_flag_set)
   call ieee_get_flag(ieee_inexact, inexact_flag_set)
   call ieee_get_flag(ieee_invalid, invalid_flag_set)
   real128_underflow_detected = underflow_flag_set
   call ieee_set_flag(ieee_overflow, .false.)
   call ieee_set_flag(ieee_underflow, .false.)
   call ieee_set_flag(ieee_divide_by_zero, .false.)
   call ieee_set_flag(ieee_inexact, .false.)
   call ieee_set_flag(ieee_invalid, .false.)

   call ieee_get_flag(ieee_overflow, overflow_flag_set)
   call ieee_get_flag(ieee_underflow, underflow_flag_set)
   call ieee_get_flag(ieee_divide_by_zero, divide_by_zero_flag_set)
   call ieee_get_flag(ieee_inexact, inexact_flag_set)
   call ieee_get_flag(ieee_invalid, invalid_flag_set)
   if ( &
           overflow_flag_set .or. &
           underflow_flag_set .or. &
           divide_by_zero_flag_set .or. &
           inexact_flag_set .or. &
           invalid_flag_set &
   ) then
      error stop 'Failed to reset IEEE exception flag.'
   end if
   call test_real128_ieee_divide_by_zero
   call ieee_get_flag(ieee_overflow, overflow_flag_set)
   call ieee_get_flag(ieee_underflow, underflow_flag_set)
   call ieee_get_flag(ieee_divide_by_zero, divide_by_zero_flag_set)
   call ieee_get_flag(ieee_inexact, inexact_flag_set)
   call ieee_get_flag(ieee_invalid, invalid_flag_set)
   real128_divide_by_zero_detected = divide_by_zero_flag_set
   call ieee_set_flag(ieee_overflow, .false.)
   call ieee_set_flag(ieee_underflow, .false.)
   call ieee_set_flag(ieee_divide_by_zero, .false.)
   call ieee_set_flag(ieee_inexact, .false.)
   call ieee_set_flag(ieee_invalid, .false.)

   call ieee_get_flag(ieee_overflow, overflow_flag_set)
   call ieee_get_flag(ieee_underflow, underflow_flag_set)
   call ieee_get_flag(ieee_divide_by_zero, divide_by_zero_flag_set)
   call ieee_get_flag(ieee_inexact, inexact_flag_set)
   call ieee_get_flag(ieee_invalid, invalid_flag_set)
   if ( &
           overflow_flag_set .or. &
           underflow_flag_set .or. &
           divide_by_zero_flag_set .or. &
           inexact_flag_set .or. &
           invalid_flag_set &
   ) then
      error stop 'Failed to reset IEEE exception flag.'
   end if
   call test_real128_ieee_invalid
   call ieee_get_flag(ieee_overflow, overflow_flag_set)
   call ieee_get_flag(ieee_underflow, underflow_flag_set)
   call ieee_get_flag(ieee_divide_by_zero, divide_by_zero_flag_set)
   call ieee_get_flag(ieee_inexact, inexact_flag_set)
   call ieee_get_flag(ieee_invalid, invalid_flag_set)
   real128_invalid_detected = invalid_flag_set
   call ieee_set_flag(ieee_overflow, .false.)
   call ieee_set_flag(ieee_underflow, .false.)
   call ieee_set_flag(ieee_divide_by_zero, .false.)
   call ieee_set_flag(ieee_inexact, .false.)
   call ieee_set_flag(ieee_invalid, .false.)

   call ieee_get_flag(ieee_overflow, overflow_flag_set)
   call ieee_get_flag(ieee_underflow, underflow_flag_set)
   call ieee_get_flag(ieee_divide_by_zero, divide_by_zero_flag_set)
   call ieee_get_flag(ieee_inexact, inexact_flag_set)
   call ieee_get_flag(ieee_invalid, invalid_flag_set)
   if ( &
           overflow_flag_set .or. &
           underflow_flag_set .or. &
           divide_by_zero_flag_set .or. &
           inexact_flag_set .or. &
           invalid_flag_set &
   ) then
      error stop 'Failed to reset IEEE exception flag.'
   end if
   call test_log_negative
   call ieee_get_flag(ieee_overflow, overflow_flag_set)
   call ieee_get_flag(ieee_underflow, underflow_flag_set)
   call ieee_get_flag(ieee_divide_by_zero, divide_by_zero_flag_set)
   call ieee_get_flag(ieee_inexact, inexact_flag_set)
   call ieee_get_flag(ieee_invalid, invalid_flag_set)
   log_negative_detected = invalid_flag_set
   call ieee_set_flag(ieee_overflow, .false.)
   call ieee_set_flag(ieee_underflow, .false.)
   call ieee_set_flag(ieee_divide_by_zero, .false.)
   call ieee_set_flag(ieee_inexact, .false.)
   call ieee_set_flag(ieee_invalid, .false.)

   if ( &
         .not. ( &
            int32_supported .and. &
            real64_supported .and. &
            int_is_int32 .and. &
            dp_is_real64 .and. &
            real64_overflow_supported .and. &
            real64_underflow_supported .and. &
            real64_divide_by_zero_supported .and. &
            real64_inexact_supported .and. &
            real64_invalid_supported .and. &
            real64_ieee_standard_supported .and. &
            real64_overflow_detected .and. &
            real64_underflow_detected .and. &
            real64_divide_by_zero_detected .and. &
            real64_invalid_detected .and. &
            log_negative_detected &
          ) &
    ) then
      error stop 'Minimal collection of IEEE features not supported.'
   end if

end program test_ieee

subroutine test_real64_ieee_overflow
   use, intrinsic :: ieee_arithmetic
   use, intrinsic :: ieee_features
   use, intrinsic :: iso_fortran_env
   real(real64) :: a = 1.0_real64
   integer(int64) :: i
   multiply_and_divide: do i = 1_int64,10000_int64
      a = a * 2.0_real64
      check_a_finite: if (.not. ieee_is_finite(a)) then
         exit
      end if check_a_finite
   end do multiply_and_divide
end subroutine test_real64_ieee_overflow

subroutine test_real64_ieee_underflow
   use, intrinsic :: ieee_arithmetic
   use, intrinsic :: ieee_features
   use, intrinsic :: iso_fortran_env
   real(real64) :: a = 1.0_real64
   integer(int64) :: i
   multiply_and_divide: do i = 1_int64,10000_int64
      a = a / 2.0_real64
      if (a == 0.0_real64) then
         exit
      end if
   end do multiply_and_divide
end subroutine test_real64_ieee_underflow

subroutine test_real64_ieee_divide_by_zero
   use, intrinsic :: ieee_arithmetic
   use, intrinsic :: ieee_features
   use, intrinsic :: iso_fortran_env
   real(real64) :: a = 1.0_real64
   real(real64) :: b = 0.0_real64
   real(real64) :: c
   c = a / b
end subroutine test_real64_ieee_divide_by_zero

subroutine test_real64_ieee_invalid
   use, intrinsic :: ieee_arithmetic
   use, intrinsic :: ieee_features
   use, intrinsic :: iso_fortran_env
   real(real64) :: a = 0.0_real64
   real(real64) :: b = 0.0_real64
   real(real64) :: c
   c = a / b
end subroutine test_real64_ieee_invalid

subroutine test_real128_ieee_overflow
   use, intrinsic :: ieee_arithmetic
   use, intrinsic :: ieee_features
   use, intrinsic :: iso_fortran_env
   real(real128) :: a = 1.0_real128
   integer(int64) :: i
   multiply_and_divide: do i = 1_int64,100000_int64
      a = a * 2.0_real128
      check_a_finite: if (.not. ieee_is_finite(a)) then
         exit
      end if check_a_finite
   end do multiply_and_divide
end subroutine test_real128_ieee_overflow

subroutine test_real128_ieee_underflow
   use, intrinsic :: ieee_arithmetic
   use, intrinsic :: ieee_features
   use, intrinsic :: iso_fortran_env
   real(real128) :: a = 1.0_real128
   integer(int64) :: i
   multiply_and_divide: do i = 1_int64,100000_int64
      a = a / 2.0_real128
      if (a == 0.0_real128) then
         exit
      end if
   end do multiply_and_divide
end subroutine test_real128_ieee_underflow

subroutine test_real128_ieee_divide_by_zero
   use, intrinsic :: ieee_arithmetic
   use, intrinsic :: ieee_features
   use, intrinsic :: iso_fortran_env
   real(real128) :: a = 1.0_real128
   real(real128) :: b = 0.0_real128
   real(real128) :: c
   c = a / b
end subroutine test_real128_ieee_divide_by_zero

subroutine test_real128_ieee_invalid
   use, intrinsic :: ieee_arithmetic
   use, intrinsic :: ieee_features
   use, intrinsic :: iso_fortran_env
   real(real128) :: a = 0.0_real128
   real(real128) :: b = 0.0_real128
   real(real128) :: c
   c = a / b
end subroutine test_real128_ieee_invalid

subroutine test_log_negative
   use, intrinsic :: ieee_arithmetic
   use, intrinsic :: ieee_features
   use, intrinsic :: iso_fortran_env
   real(real64) :: a, b
   a = -1.0_real64
   b = log(a)
end subroutine test_log_negative
