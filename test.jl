include("OptimalDesign.jl")
import LinearAlgebra as LA
import .OptimalDesign as OD

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
result = map(i -> OD.weighted_sample(weights), 1:trials)
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
@assert all(abs.(resid) .< tol)

p = 0.63
trials = 10000
p_hat = sum(map(i -> OD.bernoulli_trial(p), 1:trials)) / trials
resid = p - p_hat
@assert abs(resid) < tol

A = Float64.([3/4 -1/4 0; -1/4 3/4 0; 0 0 1/4])
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
result = map(i -> OD.sample_L_ens(A, tol), 1:trials)
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
@assert all(abs.(resid) .< tol)
