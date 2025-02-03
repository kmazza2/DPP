all : test_ieee test_lapack test_matrix_market test_matrix test_leak test_main
clean: .
	rm test_ieee test_lapack test_matrix_market test_matrix test_leak test_main lib/matrix_market.o lib/matrix_market_subs.o lib/matrix.o matrix.mod matrix_market.mod
test_ieee:	test_ieee.f90
	gfortran -std=f2018 -Wall -pedantic-errors -Wextra -Wsurprising -fsanitize=undefined -o test_ieee test_ieee.f90
test_lapack:	test_lapack.f90
	gfortran -std=f2018 -o test_lapack test_lapack.f90 -L lib -llapack -lrefblas -ltmglib
test_matrix_market:	test_matrix_market.f90 matrix_market.f90 matrix_market_subs.f
	gfortran -std=f2018 -o lib/matrix_market_subs.o -c matrix_market_subs.f
	gfortran -std=f2018 -o lib/matrix_market.o -c matrix_market.f90
	gfortran -std=f2018 -o test_matrix_market test_matrix_market.f90 -L lib -l:matrix_market.o -l:matrix_market_subs.o
matrix_market_subs.o:	matrix_market_subs.f
	gfortran -std=f2018 -o lib/matrix_market_subs.o -c matrix_market_subs.f
matrix_market.o:	matrix_market.f90 matrix_market_subs.o
	gfortran -std=f2018 -o lib/matrix_market.o -c matrix_market.f90
matrix.o:	matrix.f90 matrix_market.o
	gfortran -std=f2018 -o lib/matrix.o -c matrix.f90 -L lib -l:matrix_market.o
test_matrix:   test_matrix.f90 matrix.o matrix_market.o matrix_market_subs.o
	gfortran -std=f2018 -o test_matrix test_matrix.f90 -L lib -l:matrix.o -l:matrix_market.o -l:matrix_market_subs.o -llapack -lrefblas
test_leak:	test_leak.f90 matrix.o matrix_market.o matrix_market_subs.o
	gfortran -std=f2018 -fsanitize=undefined -o test_leak test_leak.f90  -L lib -l:matrix.o -l:matrix_market.o -l:matrix_market_subs.o -llapack -lrefblas
test_main:	test_main.f90
	gfortran -std=f2018 -o test_main test_main.f90
