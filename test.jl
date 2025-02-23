include("OptimalDesign.jl")
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

sub_0 = Set()
p_sub_0 = 1.0
sub_1 = Set(1)
p_sub_1 = 0.75
sub_2 = Set(2)
p_sub_2 = 0.75
sub_3 = Set(3)
p_sub_3 = 0.25
sub_12 = Set((1,2))
p_sub_12 = 0.5
sub_13 = Set((1,3))
p_sub_13 = 0.1875
sub_23 = Set((2,3))
p_sub_23 = 0.1875
sub_123 = Set((1,2,3))
p_sub_123 = 0.125
A = Float64.([3/4 -1/4 0; -1/4 3/4 0; 0 0 1/4])
trials = 10000
result = map(i -> OD.sample_DPP(A, tol), 1:trials)
p_hat_sub_0 = sum(map(s -> issubset(sub_0, s), result)) / trials
p_hat_sub_1 = sum(map(s -> issubset(sub_1, s), result)) / trials
p_hat_sub_2 = sum(map(s -> issubset(sub_2, s), result)) / trials
p_hat_sub_3 = sum(map(s -> issubset(sub_3, s), result)) / trials
p_hat_sub_12 = sum(map(s -> issubset(sub_12, s), result)) / trials
p_hat_sub_13 = sum(map(s -> issubset(sub_13, s), result)) / trials
p_hat_sub_23 = sum(map(s -> issubset(sub_23, s), result)) / trials
p_hat_sub_123 = sum(map(s -> issubset(sub_123, s), result)) / trials
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
