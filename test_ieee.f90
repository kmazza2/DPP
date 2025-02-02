program test

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

   print *, 'Real kinds: ', real_kinds
   print *, 'Integer kinds: ', integer_kinds
   print *, 'Character kinds: ', character_kinds
   print *, 'Logical kinds: ', logical_kinds

   print *, 'int32 supported? ', int32_supported
   int32_kind: if (int32_supported) then
           print *, 'int32 kind is ', int32
   end if int32_kind
   print *, 'int64 supported? ', int64_supported
   int64_kind: if (int64_supported) then
           print *, 'int64 kind is ', int64
   end if int64_kind

   print *, 'real64 supported? ', real64_supported
   real64_kind: if (real64_supported) then
           print *, 'real64 kind is ', real64
   end if real64_kind

   print *, 'real128 supported? ', real128_supported
   real128_kind: if (real128_supported) then
           print *, 'real128 kind is ', real128
   end if real128_kind

   print *, 'integer is int32? ', int_is_int32
   print *, 'double precision is real64? ', dp_is_real64


   print *, 'real64: overflow flag supported? ', real64_overflow_supported
   print *, 'real64: underflow flag supported? ', real64_underflow_supported
   print *, 'real64: divide_by_zero flag supported? ', real64_divide_by_zero_supported
   print *, 'real64: inexact flag supported? ', real64_inexact_supported
   print *, 'real64: invalid flag supported? ', real64_invalid_supported

   print *, 'real128: overflow flag supported? ', real128_overflow_supported
   print *, 'real128: underflow flag supported? ', real128_underflow_supported
   print *, 'real128: divide_by_zero flag supported? ', real128_divide_by_zero_supported
   print *, 'real128: inexact flag supported? ', real128_inexact_supported
   print *, 'real128: invalid flag supported? ', real128_invalid_supported

   print *, 'overflow: can set halting mode? ', ieee_support_halting(ieee_overflow)
   print *, 'underflow: can set halting mode? ', ieee_support_halting(ieee_underflow)
   print *, 'divide_by_zero: can set halting mode? ', ieee_support_halting(ieee_divide_by_zero)
   print *, 'inexact: can set halting mode? ', ieee_support_halting(ieee_inexact)
   print *, 'invalid: can set halting mode? ', ieee_support_halting(ieee_invalid)

   print *, 'real64 IEEE standard supported? ', real64_ieee_standard_supported
   print *, 'real128 IEEE standard supported? ', real128_ieee_standard_supported

   print *, 'real64 nearest rounding supported? ', ieee_support_rounding(ieee_nearest, x_real64)
!   print *, 'real64 away rounding supported? ', ieee_support_rounding(ieee_away, x_real64)
   print *, 'real64 to_zero rounding supported? ', ieee_support_rounding(ieee_to_zero, x_real64)
   print *, 'real64 up rounding supported? ', ieee_support_rounding(ieee_up, x_real64)
   print *, 'real64 down rounding supported? ', ieee_support_rounding(ieee_down, x_real64)

   print *, 'real128 nearest rounding supported? ', ieee_support_rounding(ieee_nearest, x_real128)
!   print *, 'real128 away rounding supported? ', ieee_support_rounding(ieee_away, x_real128)
   print *, 'real128 to_zero rounding supported? ', ieee_support_rounding(ieee_to_zero, x_real128)
   print *, 'real128 up rounding supported? ', ieee_support_rounding(ieee_up, x_real128)
   print *, 'real128 down rounding supported? ', ieee_support_rounding(ieee_down, x_real128)

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
   print *, 'overflow before real64 overflow call? ', overflow_flag_set
   print *, 'underflow before real64 overflow call? ', underflow_flag_set
   print *, 'divide_by_zero before real64 overflow call? ', divide_by_zero_flag_set
   print *, 'inexact before real64 overflow call? ', inexact_flag_set
   print *, 'invalid before real64 overflow call? ', invalid_flag_set
   if ( &
           overflow_flag_set .or. &
           underflow_flag_set .or. &
           divide_by_zero_flag_set .or. &
           inexact_flag_set .or. &
           invalid_flag_set &
   ) then
      stop 'Failed to reset IEEE exception flag.'
   end if
   call test_real64_ieee_overflow
   call ieee_get_flag(ieee_overflow, overflow_flag_set)
   call ieee_get_flag(ieee_underflow, underflow_flag_set)
   call ieee_get_flag(ieee_divide_by_zero, divide_by_zero_flag_set)
   call ieee_get_flag(ieee_inexact, inexact_flag_set)
   call ieee_get_flag(ieee_invalid, invalid_flag_set)
   real64_overflow_detected = overflow_flag_set
   print *, 'overflow after real64 overflow call? ', overflow_flag_set
   print *, 'underflow after real64 overflow call? ', underflow_flag_set
   print *, 'divide_by_zero after real64 overflow call? ', divide_by_zero_flag_set
   print *, 'inexact after real64 overflow call? ', inexact_flag_set
   print *, 'invalid after real64 overflow call? ', invalid_flag_set
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
   print *, 'overflow before real64 underflow call? ', overflow_flag_set
   print *, 'underflow before real64 underflow call? ', underflow_flag_set
   print *, 'divide_by_zero before real64 underflow call? ', divide_by_zero_flag_set
   print *, 'inexact before real64 underflow call? ', inexact_flag_set
   print *, 'invalid before real64 underflow call? ', invalid_flag_set
   if ( &
           overflow_flag_set .or. &
           underflow_flag_set .or. &
           divide_by_zero_flag_set .or. &
           inexact_flag_set .or. &
           invalid_flag_set &
   ) then
      stop 'Failed to reset IEEE exception flag.'
   end if
   call test_real64_ieee_underflow
   call ieee_get_flag(ieee_overflow, overflow_flag_set)
   call ieee_get_flag(ieee_underflow, underflow_flag_set)
   call ieee_get_flag(ieee_divide_by_zero, divide_by_zero_flag_set)
   call ieee_get_flag(ieee_inexact, inexact_flag_set)
   call ieee_get_flag(ieee_invalid, invalid_flag_set)
   real64_underflow_detected = underflow_flag_set
   print *, 'overflow after real64 underflow call? ', overflow_flag_set
   print *, 'underflow after real64 underflow call? ', underflow_flag_set
   print *, 'divide_by_zero after real64 underflow call? ', divide_by_zero_flag_set
   print *, 'inexact after real64 underflow call? ', inexact_flag_set
   print *, 'invalid after real64 underflow call? ', invalid_flag_set
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
   print *, 'overflow before real64 divide_by_zero call? ', overflow_flag_set
   print *, 'underflow before real64 divide_by_zero call? ', underflow_flag_set
   print *, 'divide_by_zero before real64 divide_by_zero call? ', divide_by_zero_flag_set
   print *, 'inexact before real64 divide_by_zero call? ', inexact_flag_set
   print *, 'invalid before real64 divide_by_zero call? ', invalid_flag_set
   if ( &
           overflow_flag_set .or. &
           underflow_flag_set .or. &
           divide_by_zero_flag_set .or. &
           inexact_flag_set .or. &
           invalid_flag_set &
   ) then
      stop 'Failed to reset IEEE exception flag.'
   end if
   call test_real64_ieee_divide_by_zero
   call ieee_get_flag(ieee_overflow, overflow_flag_set)
   call ieee_get_flag(ieee_underflow, underflow_flag_set)
   call ieee_get_flag(ieee_divide_by_zero, divide_by_zero_flag_set)
   call ieee_get_flag(ieee_inexact, inexact_flag_set)
   call ieee_get_flag(ieee_invalid, invalid_flag_set)
   real64_divide_by_zero_detected = divide_by_zero_flag_set
   print *, 'overflow after real64 divide_by_zero call? ', overflow_flag_set
   print *, 'underflow after real64 divide_by_zero call? ', underflow_flag_set
   print *, 'divide_by_zero after real64 divide_by_zero call? ', divide_by_zero_flag_set
   print *, 'inexact after real64 divide_by_zero call? ', inexact_flag_set
   print *, 'invalid after real64 divide_by_zero call? ', invalid_flag_set
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
   print *, 'overflow before real64 invalid call? ', overflow_flag_set
   print *, 'underflow before real64 invalid call? ', underflow_flag_set
   print *, 'divide_by_zero before real64 invalid call? ', divide_by_zero_flag_set
   print *, 'inexact before real64 invalid call? ', inexact_flag_set
   print *, 'invalid before real64 invalid call? ', invalid_flag_set
   if ( &
           overflow_flag_set .or. &
           underflow_flag_set .or. &
           divide_by_zero_flag_set .or. &
           inexact_flag_set .or. &
           invalid_flag_set &
   ) then
      stop 'Failed to reset IEEE exception flag.'
   end if
   call test_real64_ieee_invalid
   call ieee_get_flag(ieee_overflow, overflow_flag_set)
   call ieee_get_flag(ieee_underflow, underflow_flag_set)
   call ieee_get_flag(ieee_divide_by_zero, divide_by_zero_flag_set)
   call ieee_get_flag(ieee_inexact, inexact_flag_set)
   call ieee_get_flag(ieee_invalid, invalid_flag_set)
   real64_invalid_detected = invalid_flag_set
   print *, 'overflow after real64 invalid call? ', overflow_flag_set
   print *, 'underflow after real64 invalid call? ', underflow_flag_set
   print *, 'divide_by_zero after real64 invalid call? ', divide_by_zero_flag_set
   print *, 'inexact after real64 invalid call? ', inexact_flag_set
   print *, 'invalid after real64 invalid call? ', invalid_flag_set
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
   print *, 'overflow before real128 overflow call? ', overflow_flag_set
   print *, 'underflow before real128 overflow call? ', underflow_flag_set
   print *, 'divide_by_zero before real128 overflow call? ', divide_by_zero_flag_set
   print *, 'inexact before real128 overflow call? ', inexact_flag_set
   print *, 'invalid before real128 overflow call? ', invalid_flag_set
   if ( &
           overflow_flag_set .or. &
           underflow_flag_set .or. &
           divide_by_zero_flag_set .or. &
           inexact_flag_set .or. &
           invalid_flag_set &
   ) then
      stop 'Failed to reset IEEE exception flag.'
   end if
   call test_real128_ieee_overflow
   call ieee_get_flag(ieee_overflow, overflow_flag_set)
   call ieee_get_flag(ieee_underflow, underflow_flag_set)
   call ieee_get_flag(ieee_divide_by_zero, divide_by_zero_flag_set)
   call ieee_get_flag(ieee_inexact, inexact_flag_set)
   call ieee_get_flag(ieee_invalid, invalid_flag_set)
   real128_overflow_detected = overflow_flag_set
   print *, 'overflow after real128 overflow call? ', overflow_flag_set
   print *, 'underflow after real128 overflow call? ', underflow_flag_set
   print *, 'divide_by_zero after real128 overflow call? ', divide_by_zero_flag_set
   print *, 'inexact after real128 overflow call? ', inexact_flag_set
   print *, 'invalid after real128 overflow call? ', invalid_flag_set
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
   print *, 'overflow before real128 underflow call? ', overflow_flag_set
   print *, 'underflow before real128 underflow call? ', underflow_flag_set
   print *, 'divide_by_zero before real128 underflow call? ', divide_by_zero_flag_set
   print *, 'inexact before real128 underflow call? ', inexact_flag_set
   print *, 'invalid before real128 underflow call? ', invalid_flag_set
   if ( &
           overflow_flag_set .or. &
           underflow_flag_set .or. &
           divide_by_zero_flag_set .or. &
           inexact_flag_set .or. &
           invalid_flag_set &
   ) then
      stop 'Failed to reset IEEE exception flag.'
   end if
   call test_real128_ieee_underflow
   call ieee_get_flag(ieee_overflow, overflow_flag_set)
   call ieee_get_flag(ieee_underflow, underflow_flag_set)
   call ieee_get_flag(ieee_divide_by_zero, divide_by_zero_flag_set)
   call ieee_get_flag(ieee_inexact, inexact_flag_set)
   call ieee_get_flag(ieee_invalid, invalid_flag_set)
   real128_underflow_detected = underflow_flag_set
   print *, 'overflow after real128 underflow call? ', overflow_flag_set
   print *, 'underflow after real128 underflow call? ', underflow_flag_set
   print *, 'divide_by_zero after real128 underflow call? ', divide_by_zero_flag_set
   print *, 'inexact after real128 underflow call? ', inexact_flag_set
   print *, 'invalid after real128 underflow call? ', invalid_flag_set
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
   print *, 'overflow before real128 divide_by_zero call? ', overflow_flag_set
   print *, 'underflow before real128 divide_by_zero call? ', underflow_flag_set
   print *, 'divide_by_zero before real128 divide_by_zero call? ', divide_by_zero_flag_set
   print *, 'inexact before real128 divide_by_zero call? ', inexact_flag_set
   print *, 'invalid before real128 divide_by_zero call? ', invalid_flag_set
   if ( &
           overflow_flag_set .or. &
           underflow_flag_set .or. &
           divide_by_zero_flag_set .or. &
           inexact_flag_set .or. &
           invalid_flag_set &
   ) then
      stop 'Failed to reset IEEE exception flag.'
   end if
   call test_real128_ieee_divide_by_zero
   call ieee_get_flag(ieee_overflow, overflow_flag_set)
   call ieee_get_flag(ieee_underflow, underflow_flag_set)
   call ieee_get_flag(ieee_divide_by_zero, divide_by_zero_flag_set)
   call ieee_get_flag(ieee_inexact, inexact_flag_set)
   call ieee_get_flag(ieee_invalid, invalid_flag_set)
   real128_divide_by_zero_detected = divide_by_zero_flag_set
   print *, 'overflow after real128 divide_by_zero call? ', overflow_flag_set
   print *, 'underflow after real128 divide_by_zero call? ', underflow_flag_set
   print *, 'divide_by_zero after real128 divide_by_zero call? ', divide_by_zero_flag_set
   print *, 'inexact after real128 divide_by_zero call? ', inexact_flag_set
   print *, 'invalid after real128 divide_by_zero call? ', invalid_flag_set
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
   print *, 'overflow before real128 invalid call? ', overflow_flag_set
   print *, 'underflow before real128 invalid call? ', underflow_flag_set
   print *, 'divide_by_zero before real128 invalid call? ', divide_by_zero_flag_set
   print *, 'inexact before real128 invalid call? ', inexact_flag_set
   print *, 'invalid before real128 invalid call? ', invalid_flag_set
   if ( &
           overflow_flag_set .or. &
           underflow_flag_set .or. &
           divide_by_zero_flag_set .or. &
           inexact_flag_set .or. &
           invalid_flag_set &
   ) then
      stop 'Failed to reset IEEE exception flag.'
   end if
   call test_real128_ieee_invalid
   call ieee_get_flag(ieee_overflow, overflow_flag_set)
   call ieee_get_flag(ieee_underflow, underflow_flag_set)
   call ieee_get_flag(ieee_divide_by_zero, divide_by_zero_flag_set)
   call ieee_get_flag(ieee_inexact, inexact_flag_set)
   call ieee_get_flag(ieee_invalid, invalid_flag_set)
   real128_invalid_detected = invalid_flag_set
   print *, 'overflow after real128 invalid call? ', overflow_flag_set
   print *, 'underflow after real128 invalid call? ', underflow_flag_set
   print *, 'divide_by_zero after real128 invalid call? ', divide_by_zero_flag_set
   print *, 'inexact after real128 invalid call? ', inexact_flag_set
   print *, 'invalid after real128 invalid call? ', invalid_flag_set
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
   print *, 'overflow before log_negative call? ', overflow_flag_set
   print *, 'underflow before log_negative call? ', underflow_flag_set
   print *, 'divide_by_zero before log_negative call? ', divide_by_zero_flag_set
   print *, 'inexact before log_negative call? ', inexact_flag_set
   print *, 'invalid before log_negative call? ', invalid_flag_set
   if ( &
           overflow_flag_set .or. &
           underflow_flag_set .or. &
           divide_by_zero_flag_set .or. &
           inexact_flag_set .or. &
           invalid_flag_set &
   ) then
      stop 'Failed to reset IEEE exception flag.'
   end if
   call test_log_negative
   call ieee_get_flag(ieee_overflow, overflow_flag_set)
   call ieee_get_flag(ieee_underflow, underflow_flag_set)
   call ieee_get_flag(ieee_divide_by_zero, divide_by_zero_flag_set)
   call ieee_get_flag(ieee_inexact, inexact_flag_set)
   call ieee_get_flag(ieee_invalid, invalid_flag_set)
   log_negative_detected = invalid_flag_set
   print *, 'overflow after log_negative call? ', overflow_flag_set
   print *, 'underflow after log_negative call? ', underflow_flag_set
   print *, 'divide_by_zero after log_negative call? ', divide_by_zero_flag_set
   print *, 'inexact after log_negative call? ', inexact_flag_set
   print *, 'invalid after log_negative call? ', invalid_flag_set
   call ieee_set_flag(ieee_overflow, .false.)
   call ieee_set_flag(ieee_underflow, .false.)
   call ieee_set_flag(ieee_divide_by_zero, .false.)
   call ieee_set_flag(ieee_inexact, .false.)
   call ieee_set_flag(ieee_invalid, .false.)

   print *, ''
   print *, '         SUMMARY'
   print *, '--------------------------'
   if (int32_supported) then
      print *, 'int32 IS supported'
   else
      print *, 'int32 IS NOT supported'
   end if
   if (int64_supported) then
      print *, 'int64 IS supported'
   else
      print *, 'int64 IS NOT supported'
   end if
   if (real64_supported) then
      print *, 'real64 IS supported'
   else
      print *, 'real64 IS NOT supported'
   end if
   if (real128_supported) then
      print *, 'real128 IS supported'
   else
      print *, 'real128 IS NOT supported'
   end if
   if (int32_supported .and. int_is_int32) then
      print *, 'integer IS int32'
   else
      print *, 'integer IS NOT int32'
   end if
   if (real64_supported .and. dp_is_real64) then
      print *, 'double precision IS real64'
   else
      print *, 'double precision IS NOT real64'
   end if

   if (real64_overflow_supported .and. real64_overflow_detected) then
      print *, 'IEEE overflow exception IS detected for real64'
   else
      print *, 'IEEE overflow exception IS NOT detected for real64'
   end if
   if (real64_underflow_supported .and. real64_underflow_detected) then
      print *, 'IEEE underflow exception IS detected for real64'
   else
      print *, 'IEEE underflow exception IS NOT detected for real64'
   end if
   if (real64_divide_by_zero_supported .and. real64_divide_by_zero_detected) then
      print *, 'IEEE divide_by_zero exception IS detected for real64'
   else
      print *, 'IEEE divide_by_zero exception IS NOT detected for real64'
   end if
   if (real64_invalid_supported .and. real64_invalid_detected) then
      print *, 'IEEE invalid exception IS detected for real64'
   else
      print *, 'IEEE invalid exception IS NOT detected for real64'
   end if

   if (real128_overflow_supported .and. real128_overflow_detected) then
      print *, 'IEEE overflow exception IS detected for real128'
   else
      print *, 'IEEE overflow exception IS NOT detected for real128'
   end if
   if (real128_underflow_supported .and. real128_underflow_detected) then
      print *, 'IEEE underflow exception IS detected for real128'
   else
      print *, 'IEEE underflow exception IS NOT detected for real128'
   end if
   if (real128_divide_by_zero_supported .and. real128_divide_by_zero_detected) then
      print *, 'IEEE divide_by_zero exception IS detected for real128'
   else
      print *, 'IEEE divide_by_zero exception IS NOT detected for real128'
   end if
   if (real128_invalid_supported .and. real128_invalid_detected) then
      print *, 'IEEE invalid exception IS detected for real128'
   else
      print *, 'IEEE invalid exception IS NOT detected for real128'
   end if

end program test

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
