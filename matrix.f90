module matrix
   use, intrinsic :: iso_fortran_env
   use, intrinsic :: ieee_arithmetic
   use, intrinsic :: ieee_features
   ! Dense Real64Matrix
   ! NEVER ASSIGN to dr64m%array outside this module
   ! TODO: FROM_MTX
   type dr64m
      real(real64), allocatable, dimension(:,:) :: array
      integer(int32), dimension(2) :: shape
   end type dr64m
   type dr64m_svd_result
      type(dr64m) :: U, S, VT
      logical :: overflow, underflow, divide_by_zero, invalid, successful
   end type dr64m_svd_result
   interface operator(+)
      module procedure dr64m_add_dr64m
   end interface operator(+)
   interface operator(*)
      module procedure dr64m_mul_dr64m
      module procedure real64_mul_dr64m
   end interface operator(*)
contains
   function new_dr64m(arr)
      type(dr64m) :: new_dr64m
      real(real64), dimension(:,:) :: arr
      integer(int32) :: rows, columns
      rows = size(arr, 1)
      columns = size(arr, 2)
      allocate (new_dr64m%array(rows, columns))
      new_dr64m%array = arr
      new_dr64m%shape(1) = rows
      new_dr64m%shape(2) = columns
   end function new_dr64m
   function dr64m_add_dr64m(m1, m2)
      type(dr64m) :: dr64m_add_dr64m
      type(dr64m), intent(in) :: m1, m2
      if ( &
         (size(m1%array, 1) /= size(m2%array, 1)) .or. &
         (size(m1%array, 2) /= size(m2%array, 2)) &
      ) then
         stop 'Attempted to add matrices with different shapes.'
      end if
      dr64m_add_dr64m = new_dr64m(m1%array + m2%array)
   end function dr64m_add_dr64m
   function dr64m_mul_dr64m(m1, m2)
      type(dr64m) :: dr64m_mul_dr64m
      type(dr64m), intent(in) :: m1, m2
      if (size(m1%array, 2) /= size(m2%array, 1)) then
         stop 'Attempted to multiply matrices of incompatible shapes.'
      end if
      dr64m_mul_dr64m = new_dr64m(matmul(m1%array, m2%array))
   end function dr64m_mul_dr64m
   function real64_mul_dr64m(a, m)
      type(dr64m) :: real64_mul_dr64m
      real(real64), intent(in) :: a
      type(dr64m), intent(in) :: m
      real64_mul_dr64m = new_dr64m(a * m%array)
   end function real64_mul_dr64m
   function dr64m_svd(mat)

      type(dr64m_svd_result) :: dr64m_svd
      type(dr64m), intent(in) :: mat
      type(dr64m) :: mat_copy
      integer(int32) :: M, N
      integer(int32) :: LDA, LDU, LDVT
      integer(int32) :: LWMAX
      integer(int32) :: INFO, LWORK
      real(real64), allocatable, dimension(:,:) :: U, S, VT
      real(real64), allocatable, dimension(:) :: S_lapack, WORK
      external DGESVD
      intrinsic INT, MIN
      integer(int32) :: i


      M = size(mat%array, 1)
      N = size(mat%array, 2)
      mat_copy = new_dr64m(mat%array)
      LDA = M
      LDU = M
      LDVT = N
      allocate (S_lapack(N))
      allocate (WORK(1))
      allocate (U(M, M))
      allocate (S(M, N))
      allocate (VT(N, N))
      
      LWORK = -1
      call DGESVD( 'All', 'All', M, N, mat_copy%array, LDA, S_lapack, U, LDU, VT, LDVT, WORK, LWORK, INFO )
      LWORK = INT( WORK( 1 ) )
      if (allocated(WORK)) then
         deallocate(WORK)
      end if
      allocate (WORK(LWORK))
      call DGESVD( 'All', 'All', M, N, mat_copy%array, LDA, S_lapack, U, LDU, VT, LDVT, WORK, LWORK, INFO )
      do i = 1, min(M,N)
         S(i,i) = S_lapack(i)
      end do

      dr64m_svd%U = new_dr64m(U)
      dr64m_svd%S = new_dr64m(S)
      dr64m_svd%VT = new_dr64m(VT)

      call ieee_get_flag(ieee_overflow, dr64m_svd%overflow)
      call ieee_get_flag(ieee_underflow, dr64m_svd%underflow)
      call ieee_get_flag(ieee_divide_by_zero, dr64m_svd%divide_by_zero)
      call ieee_get_flag(ieee_invalid, dr64m_svd%invalid)

      if( INFO .gt. 0 ) then
         dr64m_svd%successful = .false.
      else
         dr64m_svd%successful = .true.
      end if

   end function dr64m_svd
   function dr64m_from_mtx(file_name)
      use :: matrix_market
      implicit none
      character(len=*) :: file_name
      type(dr64m) :: dr64m_from_mtx
      integer(int32) :: unit
      integer nnzmax
      character rep*10
      character field*7
      character symm*19
      character ifile*32,ofile*32
      parameter (nnzmax=100000)
      integer ival(nnzmax)
      double precision rval(nnzmax)
      complex cval(nnzmax)
      integer indx(nnzmax)
      integer jndx(nnzmax)
      integer(int32) :: i, nrows, ncols, nnz
      open(file=file_name,newunit=unit,status='old')
      call mminfo(unit,rep,field,symm,nrows,ncols,nnz)
      call mmread(unit,rep,field,symm,nrows,ncols,nnz,nnzmax, indx,jndx,ival,rval,cval)
      if( .not. ((rep .eq. 'array') .and. (field .eq. 'real')) ) then
         stop 'dr64m_from_mtx only supports dense real matrices'
      endif
      dr64m_from_mtx = new_dr64m(reshape(source=rval(:nrows*ncols), shape=[nrows, ncols]))
   end function dr64m_from_mtx
   subroutine print_dr64m( A )
      use, intrinsic :: iso_fortran_env
      type(dr64m) :: A
      integer(int32) :: M, N
      integer(int32) :: I, J
      M = size(A%array, 1)
      N = size(A%array, 2)

      do I = 1, M
      write(*,'(11(:,1X,F6.2))') ( A%array( I, J ), J = 1, N )
      end do
      return
   end subroutine print_dr64m
   function random_dr64m(rows, columns)
      integer(int32) :: rows, columns
      type(dr64m) :: random_dr64m
      real(real64), allocatable, dimension(:,:) :: arr
      if (.not. (rows > 0 .and. columns > 0)) then
         stop 'random_dr64m requires rows, columns > 0'
      end if
      allocate (arr(rows, columns))
      call random_number(arr)
      random_dr64m = new_dr64m(arr)
   end function random_dr64m

end module matrix
