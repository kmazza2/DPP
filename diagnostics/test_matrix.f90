program test_matrix

   use, intrinsic :: iso_fortran_env
   use :: matrix

   implicit none

   type(dr64m) :: matrix1, matrix2, matrix3
   type(dr64m) :: matrix4, matrix5, matrix6
   real(real64) :: a
   type(dr64m) :: matrix7, matrix8
   type(dr64m) :: matrix9
   type(dr64m_svd_result) :: svd
   real(real64), dimension(2,3) :: matrix10_array
   type(dr64m) :: matrix10
   type(dr64m) :: matrix11, matrix12
   type(dr64m) :: matrix13
   type(dr64m) :: matrix14, matrix15
   type(dr64m) :: matrix16, matrix17, matrix18

   matrix1 = new_dr64m(reshape(source = [ &
           1.0_real64, 4.0_real64, 2.0_real64, 5.0_real64, 3.0_real64, 6.0_real64 &
           ], shape = [2, 3]))
   matrix2 = new_dr64m(reshape(source = [ 2.0_real64, 5.0_real64, 3.0_real64, &
           6.0_real64, 4.0_real64, 7.0_real64], shape = [2, 3]))
   matrix3 = matrix1 + matrix2
   print *, 'matrix3 is'
   call print_dr64m(matrix3)

   matrix4 = new_dr64m(reshape(source = [ 1.0_real64, 4.0_real64, 2.0_real64, &
           5.0_real64, 3.0_real64, 6.0_real64], shape = [2, 3]))
   matrix5 = new_dr64m(reshape(source = [ 2.0_real64, 5.0_real64, 3.0_real64, &
           6.0_real64, 4.0_real64, 7.0_real64], shape = [3, 2]))
   matrix6 = matrix4 * matrix5
   print *, 'matrix6 is'
   call print_dr64m(matrix6)

   a = 2.0_real64
   matrix7 = new_dr64m(reshape(source = [ 1.0_real64, 4.0_real64, 2.0_real64, &
           5.0_real64, 3.0_real64, 6.0_real64], shape = [2, 3]))
   matrix8 = a * matrix7
   print *, 'matrix8 is'
   call print_dr64m(matrix8)

   matrix9 = new_dr64m(reshape(source = [5.0_real64, 7.0_real64, 6.0_real64, &
           5.0_real64, 7.0_real64, 10.0_real64, 8.0_real64, 7.0_real64, &
           6.0_real64, 8.0_real64, 10.0_real64, 9.0_real64, 5.0_real64, &
           7.0_real64, 9.0_real64, 10.0_real64], shape = [4, 4]))
   svd = dr64m_svd(matrix9)
   print *, 'matrix9 is'
   call print_dr64m(matrix9)
   print *, 'U: '
   call print_dr64m(svd%U)
   print *, 'S: '
   call print_dr64m(svd%S)
   print *, 'VT: '
   call print_dr64m(svd%VT)
   print *, 'overflow: ', svd%overflow
   print *, 'underflow: ', svd%underflow
   print *, 'divide_by_zero: ', svd%divide_by_zero
   print *, 'invalid: ', svd%invalid
   print *, 'successful: ', svd%successful
   print *, 'Do we get the original matrix?'
   call print_dr64m(svd%U * svd%S * svd%VT)

   matrix10 = new_dr64m( &
      reshape( &
         source = [ &
            1.0_real64, 4.0_real64, 2.0_real64, 5.0_real64, &
            3.0_real64, 6.0_real64 &
         ], shape = [2, 3] &
      ) &
   )
   print *, 'matrix 10 is'
   call print_dr64m(matrix10)
   print *, 'rows in matrix10: ', matrix10%shape(1)
   print *, 'columns in matrix10: ', matrix10%shape(2)

   print *, 'About to try reading matrix from test_real_mat.mtx...'
   matrix11 = dr64m_from_mtx('test_real_mat.mtx')
   print *, 'Read matrix from test_real_mat.mtx.'
   call print_dr64m(matrix11)

   matrix13 = random_dr64m(5,3)
   print *, 'matrix13, random 5 x 3 matrix:'
   call print_dr64m(matrix13)

   print *, 'What happens if I use an elemental?'
   print *, 'multiply by 2 * pi:'
   matrix13 = 2 * 3.1415926535897932384626433832795_real64 * matrix13
   call print_dr64m(matrix13)
   print *, 'call sin:'
   print *, sin(matrix13%array)
   print *, 'matrix13:'
   call print_dr64m(matrix13)

   matrix14 = new_dr64m(reshape(source = [ 1.0_real64, 3.0_real64, 2.0_real64, 4.0_real64 ], shape = [2, 2]))
   matrix15 = new_dr64m(reshape(source = [ 2.0_real64, 3.0_real64, 2.0_real64, 4.0_real64 ], shape = [2, 2]))
   print *, all(matrix14%array == matrix15%array)

   matrix16 = new_dr64m(reshape(source = [ &
           1.0_real64, 4.0_real64, 2.0_real64, 5.0_real64, 3.0_real64, 6.0_real64 &
           ], shape = [2, 3]))
   matrix17 = get_dr64m_row(matrix16, 2)
   matrix18 = get_dr64m_col(matrix16, 3)
   call print_dr64m(matrix17)
   call print_dr64m(matrix18)

end program test_matrix
