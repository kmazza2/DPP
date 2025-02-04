all : diagnostics/test_ieee diagnostics/test_lapack diagnostics/test_matrix_market diagnostics/test_matrix diagnostics/test_leak test/test_ieee test/test_fail test/test_get_dr64m_row test/test_get_dr64m_col
clean: .
	rm diagnostics/test_ieee diagnostics/test_lapack diagnostics/test_matrix_market diagnostics/test_matrix diagnostics/test_leak test/test_ieee test/test_fail test/test_get_dr64m_row test/test_get_dr64m_col lib/matrix_market.o lib/matrix_market_subs.o lib/matrix.o matrix.mod matrix_market.mod
diagnostics/test_ieee:	diagnostics/test_ieee.f90
	gfortran -std=f2018 -Wall -pedantic-errors -Wextra -Wsurprising -fsanitize=undefined -o diagnostics/test_ieee diagnostics/test_ieee.f90
diagnostics/test_lapack:	diagnostics/test_lapack.f90
	gfortran -std=f2018 -o diagnostics/test_lapack diagnostics/test_lapack.f90 -L lib -llapack -lrefblas -ltmglib
diagnostics/test_matrix_market:	diagnostics/test_matrix_market.f90 matrix_market.f90 matrix_market_subs.f
	gfortran -std=f2018 -o lib/matrix_market_subs.o -c matrix_market_subs.f
	gfortran -std=f2018 -o lib/matrix_market.o -c matrix_market.f90
	gfortran -std=f2018 -o diagnostics/test_matrix_market diagnostics/test_matrix_market.f90 -L lib -l:matrix_market.o -l:matrix_market_subs.o
matrix_market_subs.o:	matrix_market_subs.f
	gfortran -std=f2018 -o lib/matrix_market_subs.o -c matrix_market_subs.f
matrix_market.o:	matrix_market.f90 matrix_market_subs.o
	gfortran -std=f2018 -o lib/matrix_market.o -c matrix_market.f90
matrix.o:	matrix.f90 matrix_market.o
	gfortran -std=f2018 -o lib/matrix.o -c matrix.f90 -L lib -l:matrix_market.o
diagnostics/test_matrix:   diagnostics/test_matrix.f90 matrix.o matrix_market.o matrix_market_subs.o
	gfortran -std=f2018 -o diagnostics/test_matrix diagnostics/test_matrix.f90 -L lib -l:matrix.o -l:matrix_market.o -l:matrix_market_subs.o -llapack -lrefblas
diagnostics/test_leak:	diagnostics/test_leak.f90 matrix.o matrix_market.o matrix_market_subs.o
	gfortran -std=f2018 -fsanitize=undefined -o diagnostics/test_leak diagnostics/test_leak.f90  -L lib -l:matrix.o -l:matrix_market.o -l:matrix_market_subs.o -llapack -lrefblas
test/test_ieee:	test/test_ieee.f90
	gfortran -std=f2018 -o test/test_ieee test/test_ieee.f90
test/test_fail:	test/test_fail.f90
	gfortran -std=f2018 -o test/test_fail test/test_fail.f90
test/test_get_dr64m_row:	test/test_get_dr64m_row.f90 matrix.o
	gfortran -std=f2018 -o test/test_get_dr64m_row test/test_get_dr64m_row.f90 -L lib -l:matrix.o -l:matrix_market.o -l:matrix_market_subs.o -llapack -lrefblas
test/test_get_dr64m_col:	test/test_get_dr64m_col.f90 matrix.o
	gfortran -std=f2018 -o test/test_get_dr64m_col test/test_get_dr64m_col.f90 -L lib -l:matrix.o -l:matrix_market.o -l:matrix_market_subs.o -llapack -lrefblas
