use, intrinsic :: iso_fortran_env

integer M, N
parameter        ( M = 2, N = 2 )
integer          LDA, LDU, LDVT
parameter        ( LDA = M, LDU = M, LDVT = N )
integer          LWMAX
parameter        ( LWMAX = 1000 )
integer          INFO, LWORK
real(real64) A( LDA, N ), U( LDU, M ), VT( LDVT, N ), S( N ), WORK( LWMAX )
data             A/ 1, 2, 3, 4 /
external DGESVD
external PRINT_MATRIX
intrinsic        INT, MIN

real(real64), parameter :: sv1 = 5.46499_real64
real(real64), parameter :: sv2 = 0.365966_real64
real(real64), parameter :: lsv1_1 = -0.576048_real64
real(real64), parameter :: lsv1_2 = -0.817416_real64
real(real64), parameter :: lsv2_1 = -0.817416_real64
real(real64), parameter :: lsv2_2 = 0.576048_real64
real(real64), parameter :: rsv1_1 = -0.404554_real64
real(real64), parameter :: rsv1_2 = -0.914514_real64
real(real64), parameter :: rsv2_1 = 0.914514_real64
real(real64), parameter :: rsv2_2 = -0.404554_real64
real(real64), parameter :: tol = 0.1_real64
real(real64) :: sv1_resid, sv2_resid
real(real64) :: lsv1_1_resid, lsv1_2_resid, lsv2_1_resid, lsv2_2_resid
real(real64) :: rsv1_1_resid, rsv1_2_resid, rsv2_1_resid, rsv2_2_resid
logical :: sv1_within_tol
logical :: sv2_within_tol
logical :: lsv1_1_within_tol
logical :: lsv1_2_within_tol
logical :: lsv2_1_within_tol
logical :: lsv2_2_within_tol
logical :: rsv1_1_within_tol
logical :: rsv1_2_within_tol
logical :: rsv2_1_within_tol
logical :: rsv2_2_within_tol
write(*,*) 'DGESVD Example Program Results'
LWORK = -1
call DGESVD( 'All', 'All', M, N, A, LDA, S, U, LDU, VT, LDVT, WORK, LWORK, INFO )
LWORK = MIN( LWMAX, INT( WORK( 1 ) ) )
call DGESVD( 'All', 'All', M, N, A, LDA, S, U, LDU, VT, LDVT, WORK, LWORK, INFO )
if( INFO .gt. 0 ) then
   write(*,*) 'The algorithm computing SVD failed to converge.'
   stop
end if 

sv1_resid = abs(sv1 - S(1))
sv2_resid = abs(sv2 - S(2))
lsv1_1_resid = abs(lsv1_1 - U(1,1))
lsv1_2_resid = abs(lsv1_2 - U(2,1))
lsv2_1_resid = abs(lsv2_1 - U(1,2))
lsv2_2_resid = abs(lsv2_2 - U(2,2))
rsv1_1_resid = abs(rsv1_1 - VT(1,1))
rsv1_2_resid = abs(rsv1_2 - VT(1,2))
rsv2_1_resid = abs(rsv2_1 - VT(2,1))
rsv2_2_resid = abs(rsv2_2 - VT(2,2))
sv1_within_tol = sv1_resid < tol
sv2_within_tol = sv2_resid < tol
lsv1_1_within_tol = lsv1_1_resid < tol
lsv1_2_within_tol = lsv1_2_resid < tol
lsv2_1_within_tol = lsv2_1_resid < tol
lsv2_2_within_tol = lsv2_2_resid < tol
rsv1_1_within_tol = rsv1_1_resid < tol
rsv1_2_within_tol = rsv1_2_resid < tol
rsv2_1_within_tol = rsv2_1_resid < tol
rsv2_2_within_tol = rsv2_2_resid < tol

call print_matrix( 'Singular values', 1, N, S, 1 )
call print_matrix( 'Left singular vectors (stored columnwise)', M, N, U, LDU )
call print_matrix( 'Right singular vectors (stored rowwise)', N, N, VT, LDVT )

print *, ''
print *, '   SUMMARY   '
print *, '-------------'
if (sv1_within_tol) then
   print *, 'sv1 IS within tolerance'
else
   print *, 'sv1 IS NOT within tolerance'
end if
if (sv2_within_tol) then
   print *, 'sv2 IS within tolerance'
else
   print *, 'sv2 IS NOT within tolerance'
end if
if (lsv1_1_within_tol) then
   print *, 'lsv1_1 IS within tolerance'
else
   print *, 'lsv1_1 IS NOT within tolerance'
end if
if (lsv1_2_within_tol) then
   print *, 'lsv1_2 IS within tolerance'
else
   print *, 'lsv1_2 IS NOT within tolerance'
end if
if (lsv2_1_within_tol) then
   print *, 'lsv2_1 IS within tolerance'
else
   print *, 'lsv2_1 IS NOT within tolerance'
end if
if (lsv2_2_within_tol) then
   print *, 'lsv2_2 IS within tolerance'
else
   print *, 'lsv2_2 IS NOT within tolerance'
end if
if (rsv1_1_within_tol) then
   print *, 'rsv1_1 IS within tolerance'
else
   print *, 'rsv1_1 IS NOT within tolerance'
end if
if (rsv1_2_within_tol) then
   print *, 'rsv1_2 IS within tolerance'
else
   print *, 'rsv1_2 IS NOT within tolerance'
end if
if (rsv2_1_within_tol) then
   print *, 'rsv2_1 IS within tolerance'
else
   print *, 'rsv2_1 IS NOT within tolerance'
end if
if (rsv2_2_within_tol) then
   print *, 'rsv2_2 IS within tolerance'
else
   print *, 'rsv2_2 IS NOT within tolerance'
end if
stop
end

subroutine print_matrix( DESC, M, N, A, LDA )
use, intrinsic :: iso_fortran_env
character*(*)    DESC
integer          M, N, LDA
real(real64) A( LDA, * )
integer          I, J
write(*,*)
write(*,*) DESC
do I = 1, M
write(*,'(11(:,1X,F6.2))') ( A( I, J ), J = 1, N )
end do
return
end
