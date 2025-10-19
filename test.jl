include("DPP.jl")
import LinearAlgebra as LA
import .DPP as OD
using Random: rand, Xoshiro

rng = Xoshiro(1234)

tol = 1e-5
standard_basis = Float64.([1 0 0; 0 1 0; 0 0 1])
e3 = Float64.([0 0 1]')
A = OD.orthogonal_subspace_basis(
    standard_basis, e3, tol
)
@assert size(A, 2) == 2

standard_basis = Float64.([1 0 0 0; 0 1 0 0; 0 0 1 0; 0 0 0 1])
e1 = Float64.([1 0 0 0]')
B = OD.orthogonal_subspace_basis(
    standard_basis, e1, tol
)
@assert size(B, 2) == 3

weights = [0.1, 0.3, 0.4, 0.2]
trials = 10000
result = map(i -> OD.weighted_sample(weights, rng), 1:trials)
p_hat_1 = sum(map(n -> isequal(n, 1), result)) / trials
p_hat_2 = sum(map(n -> isequal(n, 2), result)) / trials
p_hat_3 = sum(map(n -> isequal(n, 3), result)) / trials
p_hat_4 = sum(map(n -> isequal(n, 4), result)) / trials
tol = 1e-1
resid = [
    weights[1] - p_hat_1
    weights[2] - p_hat_2
    weights[3] - p_hat_3
    weights[4] - p_hat_4
]
@assert LA.norm(resid, Inf) < tol

p = 0.63
trials = 10000
p_hat = sum(map(i -> OD.bernoulli_trial(p, rng), 1:trials)) / trials
resid = p - p_hat
@assert abs(resid) < tol

A = Float64.(
      1  * [ -1/sqrt(2) 1/sqrt(2) 0 ]' * [ -1/sqrt(2) 1/sqrt(2) 0 ] +
    1/2  * [  1/sqrt(2) 1/sqrt(2) 0 ]' * [  1/sqrt(2) 1/sqrt(2) 0 ] +
    1/4  * [          0         0 1 ]' * [          0         0 1 ]
)
scale_factor = 1.0 / LA.det(A + LA.I)
sub_0 = Set()
p_sub_0 = 1.0 * scale_factor
sub_1 = Set(1)
p_sub_1 = 0.75 * scale_factor
sub_2 = Set(2)
p_sub_2 = 0.75 * scale_factor
sub_3 = Set(3)
p_sub_3 = 0.25 * scale_factor
sub_12 = Set((1,2))
p_sub_12 = 0.5 * scale_factor
sub_13 = Set((1,3))
p_sub_13 = 0.1875 * scale_factor
sub_23 = Set((2,3))
p_sub_23 = 0.1875 * scale_factor
sub_123 = Set((1,2,3))
p_sub_123 = 0.125 * scale_factor
trials = 10000
tol = 1e-5
result = map(i -> OD.sample_L_ensemble(A, tol, rng), 1:trials)
p_hat_sub_0 = sum(map(s -> isequal(sub_0, s), result)) / trials
p_hat_sub_1 = sum(map(s -> isequal(sub_1, s), result)) / trials
p_hat_sub_2 = sum(map(s -> isequal(sub_2, s), result)) / trials
p_hat_sub_3 = sum(map(s -> isequal(sub_3, s), result)) / trials
p_hat_sub_12 = sum(map(s -> isequal(sub_12, s), result)) / trials
p_hat_sub_13 = sum(map(s -> isequal(sub_13, s), result)) / trials
p_hat_sub_23 = sum(map(s -> isequal(sub_23, s), result)) / trials
p_hat_sub_123 = sum(map(s -> isequal(sub_123, s), result)) / trials
resid = [
    p_sub_0 - p_hat_sub_0
    p_sub_1 - p_hat_sub_1
    p_sub_2 - p_hat_sub_2
    p_sub_3 - p_hat_sub_3
    p_sub_12 - p_hat_sub_12
    p_sub_13 - p_hat_sub_13
    p_sub_23 - p_hat_sub_23
    p_sub_123 - p_hat_sub_123
]
tol = 1e-1
@assert LA.norm(resid, Inf) < tol

A = [0.0 0.0 0.0; 0.0 0.0 0.0; 0.0 0.0 0.0]
decomp = OD.orthonormal_spectral_decomposition(A, tol)
decomp_matrix = OD.orthonormal_spectral_decomposition_matrix(decomp)
resid = A - decomp_matrix
@assert LA.norm(resid, Inf) < tol

A = [-1.0 0.0 0.0; 0.0 2.0 0.0; 0.0 0.0 -3.0]
decomp = OD.orthonormal_spectral_decomposition(A, tol)
decomp_matrix = OD.orthonormal_spectral_decomposition_matrix(decomp)
resid = A - decomp_matrix
@assert LA.norm(resid, Inf) < tol

A = [1.0 6.0; 6.0 1.0]
decomp = OD.orthonormal_spectral_decomposition(A, tol)
decomp_matrix = OD.orthonormal_spectral_decomposition_matrix(decomp)
resid = A - decomp_matrix
@assert LA.norm(resid, Inf) < tol

A = [2.0 0.0 0.0; 0.0 2.0 0.0; 0.0 0.0 3.0]
decomp = OD.orthonormal_spectral_decomposition(A, tol)
sorted_eigenvalues = sort([pair.val for pair in decomp.eigenpairs])
@assert abs(sorted_eigenvalues[1] - 2.0) < tol
@assert abs(sorted_eigenvalues[2] - 2.0) < tol
@assert abs(sorted_eigenvalues[3] - 3.0) < tol

trials = 10000
set_empty = Set()
set_1 = Set([1])
set_2 = Set([2])
set_3 = Set([3])
set_4 = Set([4])
set_1_2 = Set([1 2])
set_1_3 = Set([1 3])
set_1_4 = Set([1 4])
set_2_3 = Set([2 3])
set_2_4 = Set([2 4])
set_3_4 = Set([3 4])
set_1_2_3 = Set([1 2 3])
set_1_2_4 = Set([1 2 4])
set_1_3_4 = Set([1 3 4])
set_2_3_4 = Set([2 3 4])
set_1_2_3_4 = Set([1 2 3 4])
A = [1.0;;]
tol = 1e-5
simulation = map(_ -> OD.DPP(A, tol, rng), 1:trials)
set_empty_rel_freq = sum(map(sample -> issubset(set_empty, sample), simulation)) / trials
set_1_rel_freq = sum(map(sample -> issubset(set_1, sample), simulation))  / trials
set_empty_expected_rel_freq = 1
set_1_expected_rel_freq =  1
tol = 1e-2
@assert abs(set_empty_expected_rel_freq - set_empty_rel_freq) < tol
@assert abs(set_1_expected_rel_freq - set_1_rel_freq) < tol
A = [1/2;;]
tol = 1e-5
simulation = map(_ -> OD.DPP(A, tol, rng), 1:trials)
set_empty_rel_freq = sum(map(sample -> issubset(set_empty, sample), simulation)) / trials
set_1_rel_freq = sum(map(sample -> issubset(set_1, sample), simulation)) / trials
set_empty_expected_rel_freq = 1
set_1_expected_rel_freq = 1/2
tol = 1e-2
@assert abs(set_empty_expected_rel_freq - set_empty_rel_freq) < tol
@assert abs(set_1_expected_rel_freq - set_1_rel_freq) < tol
A = [3/4 -1/4; -1/4 3/4]
tol = 1e-5
simulation = map(_ -> OD.DPP(A, tol, rng), 1:trials)
set_empty_rel_freq = sum(map(sample -> issubset(set_empty, sample), simulation)) / trials
set_1_rel_freq = sum(map(sample -> issubset(set_1, sample), simulation)) / trials
set_2_rel_freq = sum(map(sample -> issubset(set_2, sample), simulation)) / trials
set_1_2_rel_freq = sum(map(sample -> issubset(set_1_2, sample), simulation)) / trials
set_empty_expected_rel_freq = 1
set_1_expected_rel_freq = 3/4
set_2_expected_rel_freq = 3/4
set_1_2_expected_rel_freq = 1/2
tol = 1e-2
@assert abs(set_empty_expected_rel_freq - set_empty_rel_freq) < tol
@assert abs(set_1_expected_rel_freq - set_1_rel_freq) < tol
@assert abs(set_2_expected_rel_freq - set_2_rel_freq) < tol
@assert abs(set_1_2_expected_rel_freq - set_1_2_rel_freq) < tol
A = [5/16 1/16 -1/8; 1/16 5/16 -1/8; -1/8 -1/8 1/4]
tol = 1e-5
simulation = map(_ -> OD.DPP(A, tol, rng), 1:trials)
set_empty_rel_freq =sum(map(sample -> issubset(set_empty, sample), simulation)) / trials
set_1_rel_freq =sum(map(sample -> issubset(set_1, sample), simulation)) / trials
set_2_rel_freq = sum(map(sample -> issubset(set_2, sample), simulation)) / trials
set_3_rel_freq = sum(map(sample -> issubset(set_3, sample), simulation)) / trials
set_1_2_rel_freq = sum(map(sample -> issubset(set_1_2, sample), simulation)) / trials
set_1_3_rel_freq = sum(map(sample -> issubset(set_1_3, sample), simulation)) / trials
set_2_3_rel_freq = sum(map(sample -> issubset(set_2_3, sample), simulation)) / trials
set_1_2_3_rel_freq = sum(map(sample -> issubset(set_1_2_3, sample), simulation)) / trials
set_empty_expected_rel_freq = 1
set_1_expected_rel_freq = 5/16
set_2_expected_rel_freq = 5/16
set_3_expected_rel_freq = 1/4
set_1_2_expected_rel_freq = 3/32
set_1_3_expected_rel_freq = 1/16
set_2_3_expected_rel_freq = 1/16
set_1_2_3_expected_rel_freq = 1/64
tol = 1e-2
@assert abs(set_empty_expected_rel_freq - set_empty_rel_freq) < tol
@assert abs(set_1_expected_rel_freq - set_1_rel_freq) < tol
@assert abs(set_2_expected_rel_freq - set_2_rel_freq) < tol
@assert abs(set_3_expected_rel_freq - set_3_rel_freq) < tol
@assert abs(set_1_2_expected_rel_freq - set_1_2_rel_freq) < tol
@assert abs(set_1_3_expected_rel_freq - set_1_3_rel_freq) < tol
@assert abs(set_2_3_expected_rel_freq - set_2_3_rel_freq) < tol
@assert abs(set_1_2_3_expected_rel_freq - set_1_2_3_rel_freq) < tol
A = [1/6 1/18 1/18 1/18; 1/18 19/162 13/162 13/162; 1/18 13/162 17/162 5/54; 1/18 13/162 5/54 17/162]
tol = 1e-5
simulation = map(_ -> OD.DPP(A, tol, rng), 1:trials)
set_empty_rel_freq =sum(map(sample -> issubset(set_empty, sample), simulation)) / trials
set_1_rel_freq =sum(map(sample -> issubset(set_1, sample), simulation)) / trials
set_2_rel_freq = sum(map(sample -> issubset(set_2, sample), simulation)) / trials
set_3_rel_freq = sum(map(sample -> issubset(set_3, sample), simulation)) / trials
set_4_rel_freq = sum(map(sample -> issubset(set_4, sample), simulation)) / trials
set_1_2_rel_freq = sum(map(sample -> issubset(set_1_2, sample), simulation)) / trials
set_1_3_rel_freq = sum(map(sample -> issubset(set_1_3, sample), simulation)) / trials
set_1_4_rel_freq = sum(map(sample -> issubset(set_1_4, sample), simulation)) / trials
set_2_3_rel_freq = sum(map(sample -> issubset(set_2_3, sample), simulation)) / trials
set_2_4_rel_freq = sum(map(sample -> issubset(set_2_4, sample), simulation)) / trials
set_3_4_rel_freq = sum(map(sample -> issubset(set_3_4, sample), simulation)) / trials
set_1_2_3_rel_freq = sum(map(sample -> issubset(set_1_2_3, sample), simulation)) / trials
set_1_2_4_rel_freq = sum(map(sample -> issubset(set_1_2_4, sample), simulation)) / trials
set_1_3_4_rel_freq = sum(map(sample -> issubset(set_1_3_4, sample), simulation)) / trials
set_2_3_4_rel_freq = sum(map(sample -> issubset(set_2_3_4, sample), simulation)) / trials
set_1_2_3_4_rel_freq = sum(map(sample -> issubset(set_1_2_3_4, sample), simulation)) / trials
set_empty_expected_rel_freq = 1
set_1_expected_rel_freq = 1/6
set_2_expected_rel_freq = 19/162
set_3_expected_rel_freq = 17/162
set_4_expected_rel_freq = 17/162
set_1_2_expected_rel_freq = 4/243
set_1_3_expected_rel_freq = 7/486
set_1_4_expected_rel_freq = 7/486
set_2_3_expected_rel_freq = 77/13122
set_2_4_expected_rel_freq = 77/13122
set_3_4_expected_rel_freq = 16/6561
set_1_2_3_expected_rel_freq = 31/39366
set_1_2_4_expected_rel_freq = 31/39366
set_1_3_4_expected_rel_freq = 13/39366
set_2_3_4_expected_rel_freq = 5/39366
set_1_2_3_4_expected_rel_freq = 1/59049
tol = 1e-2
@assert abs(set_empty_expected_rel_freq - set_empty_rel_freq) < tol
@assert abs(set_1_expected_rel_freq - set_1_rel_freq) < tol
@assert abs(set_2_expected_rel_freq - set_2_rel_freq) < tol
@assert abs(set_3_expected_rel_freq - set_3_rel_freq) < tol
@assert abs(set_4_expected_rel_freq - set_4_rel_freq) < tol
@assert abs(set_1_2_expected_rel_freq - set_1_2_rel_freq) < tol
@assert abs(set_1_3_expected_rel_freq - set_1_3_rel_freq) < tol
@assert abs(set_1_4_expected_rel_freq - set_1_4_rel_freq) < tol
@assert abs(set_2_3_expected_rel_freq - set_2_3_rel_freq) < tol
@assert abs(set_2_4_expected_rel_freq - set_2_4_rel_freq) < tol
@assert abs(set_3_4_expected_rel_freq - set_3_4_rel_freq) < tol
@assert abs(set_1_2_3_expected_rel_freq - set_1_2_3_rel_freq) < tol
@assert abs(set_1_2_4_expected_rel_freq - set_1_2_4_rel_freq) < tol
@assert abs(set_1_3_4_expected_rel_freq - set_1_3_4_rel_freq) < tol
@assert abs(set_2_3_4_expected_rel_freq - set_2_3_4_rel_freq) < tol
@assert abs(set_1_2_3_4_expected_rel_freq - set_1_2_3_4_rel_freq) < tol

A = [1.0 -2.0; -2.0 4.0]
@assert OD.is_positive_semidefinite(A)

A = [1.0 -2.0; -2.0 -4.0]
@assert !OD.is_positive_semidefinite(A)

tol = 1e-3
A = [3.5 0.5; 0.5 3.5]
expected_sqrt_A = [1.866 0.134; 0.134 1.866]
actual_sqrt_A = OD.symmetric_sqrt(A, tol)
resid = expected_sqrt_A - actual_sqrt_A
@assert LA.norm(resid, Inf) < tol

X = [1.5 2.5; 0.2 3.0; 2.1 1.5]
p = [0.5, 0.5, 0.25]
D = LA.diagm(p)
A = [1.0 2.0; 2.0 4.0]
tol = 1e-2
trials = 10000

simulation = map(_ -> OD.regularized_DPP(X, A, p, tol, rng), 1:trials)

set_empty = Set([])
set_3 = Set([3])
set_1_2 = Set([1 2])
set_1_2_3 = Set([1 2 3])

denom = LA.det(X' * D * X + A)

num = LA.det(A)
factor = 0.5 * 0.5 * 0.75
set_empty_expected_rel_freq = num / denom * factor
set_empty_actual_rel_freq = sum(map(sample -> isequal(set_empty, sample), simulation)) / trials

num = LA.det([2.1; 1.5] * [2.1; 1.5]' + A)
factor = 0.25 * 0.5 * 0.5
set_3_expected_rel_freq = num / denom * factor
set_3_actual_rel_freq = sum(map(sample -> isequal(set_3, sample), simulation)) / trials

num = LA.det([1.5 0.2; 2.5 3.0] * [1.5 0.2; 2.5 3.0]' + A)
factor = 0.5 * 0.5 * 0.75
set_1_2_expected_rel_freq = num / denom * factor
set_1_2_actual_rel_freq = sum(map(sample -> isequal(set_1_2, sample), simulation)) / trials

num = LA.det([1.5 0.2 2.1; 2.5 3.0 1.5] * [1.5 0.2 2.1; 2.5 3.0 1.5]' + A)
factor = 0.5 * 0.5 * 0.25
set_1_2_3_expected_rel_freq = num / denom * factor
set_1_2_3_actual_rel_freq = sum(map(sample -> isequal(set_1_2_3, sample), simulation)) / trials

@assert abs(set_empty_expected_rel_freq - set_empty_actual_rel_freq) < tol
@assert abs(set_3_expected_rel_freq - set_3_actual_rel_freq) < tol
@assert abs(set_1_2_expected_rel_freq - set_1_2_actual_rel_freq) < tol
@assert abs(set_1_2_3_expected_rel_freq - set_1_2_3_actual_rel_freq) < tol
