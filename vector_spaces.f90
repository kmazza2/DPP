module vector_spaces

   use, intrinsic :: iso_fortran_env
   use, intrinsic :: ieee_arithmetic
   use, intrinsic :: ieee_features
   use :: matrix

contains

    function standard_basis(i, n)
       integer(int32) :: i, n
       type(dr64m) :: standard_basis
       real(real64), dimension(n) :: tmp
       if (n < 1_int32 .or. i < 1_int32 .or. i > n) then
          error stop 'Must have i < n.'
       end if
       tmp = 0.0_real64
       tmp(i) = 1.0_real64
       standard_basis = new_dr64m(reshape(source = tmp, shape = [n, 1]))
    end function standard_basis

    function nullspace_basis(A, tol)
       ! The columns of the matrix returned by this function should
       ! form an orthonormal basis for the nullspace of A.
       ! The tol parameter controls how small a singular value needs
       ! to be for the corresponding vector to be included in the
       ! basis.
       type(dr64m) :: nullspace_basis
       type(dr64m) :: A
       real(real64) :: tol
       type(dr64m_svd_result) :: decomp
       integer(int32) :: rows, cols, i, j, n, smallest
       type(dr64m) :: basis_vector
       decomp = dr64m_svd(A)
       rows = decomp%S%shape(1)
       cols = decomp%S%shape(2)
       n = min(rows, cols)
       smallest = 0
       find_smallest: do i = 1,n
          if (abs(decomp%S%array(i,i)) < tol) then
             smallest = i
             exit find_smallest
          end if
       end do find_smallest
       nullspace_basis = new_dr64m( &
          reshape( &
             source = [ ( 0.0_real64, j=1,( smallest * decomp%U%shape(1) ) ) ], &
             shape = [decomp%U%shape(1), smallest] &
          ) &
       )
       if (smallest > 0) then
          do i = smallest,n
             basis_vector = dr64m_transpose(get_dr64m_row(decomp%VT, i))
             nullspace_basis%array(:,(i - smallest + 1)) = &
                basis_vector%array(:,1)
          end do
       end if
    end function nullspace_basis

    function orthogonal_subspace_basis(i, V, tol)
       type(dr64m) :: orthogonal_subspace_basis
       integer(int32) :: i
       type(dr64m) :: V
       real(real64) :: tol
       error stop 'Not implemented'
    end function orthogonal_subspace_basis

end module vector_spaces
