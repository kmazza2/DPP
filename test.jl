include("OptimalDesign.jl")
import .OptimalDesign as OD

standard_basis = Float64.([1 0 0; 0 1 0; 0 0 1])
e3 = Float64.([0 0 1]')
A = OD.orthogonal_subspace_basis(
    standard_basis, e3, 1e-5
)
@assert size(A, 2) == 2

standard_basis = Float64.([1 0 0 0; 0 1 0 0; 0 0 1 0; 0 0 0 1])
e1 = Float64.([1 0 0 0]')
B = OD.orthogonal_subspace_basis(
    standard_basis, e1, 1e-5
)
@assert size(B, 2) == 3
