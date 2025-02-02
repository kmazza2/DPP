![example workflow](https://github.com/kmazza2/Optimal-Design/actions/workflows/x86_64_tests.yml/badge.svg?event=push)

A real64 dense matrix type that crashes on use of undefined operations (in particular, it will crash on attempt to multiply a 1 x 1 matrix and an m x n matrix if m /= 1). SVD is supported through dr64m_svd, and the result includes the status of the IEEE exception flags after return. The goal of this library is to force a program to crash if the programmer makes a mistake translating a formula involving matrix operations, and to make it easy to check if an algorithm on matrices is failing subtly. A scalar and a 1 x 1 matrix are not the same and neither is automatically converted to the other, a column vector and a row vector are not the same and neither is automatically converted to the other, and checking for underflow in the result of an SVD just requires checking the underflow field of the dr64m_svd_result object.

To build everything, run:
```
./configure
make
```

When working with these modules, ALWAYS use iso_fortran_env's real64 real and int32 integer kinds unless you are SURE your calculation will work with other kinds.
