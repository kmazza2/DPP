include("OptimalDesign.jl")
import .OptimalDesign as OD

standard_basis = Float64.([1 0 0; 0 1 0; 0 0 1])
e3 = Float64.([0 0 1]')
B = OD.orthogonal_subspace_basis(
    standard_basis, e3, 1e-5
)
@assert size(B, 2) == 2
