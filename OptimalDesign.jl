module OptimalDesign

export orthogonal_subspace_basis, sample_DPP

import LinearAlgebra as LA
using Random: rand, Xoshiro

rng = Xoshiro(1234)

function orthogonal_subspace_basis(A, u, tol)
    product_decomp = LA.svd(u' * A, full=true)
    nullspace_basis = product_decomp.V[
        :,
        [
            abs.(product_decomp.S);
            zeros(size(A,2) - length(product_decomp.S))
        ] .< tol
    ]
    spanning_set_decomp = LA.svd(A * nullspace_basis, full=true)
    spanning_set_basis = spanning_set_decomp.U[
        :,
        [
            abs.(spanning_set_decomp.S);
            zeros(size(A,1) - length(spanning_set_decomp.S))
        ] .> tol
    ]
    return spanning_set_basis
end

function sample_DPP(L, tol)
    @assert LA.issymmetric(L)
    v, M = LA.eigen(L)
    @assert all(v .>= 0.0)
    @assert all(v .<= 1.0)
    N = size(L)[1]
    Y = Set()
    V = M[:, map(l -> rand(rng) < l / (l + 1.0), v)]
    while size(V)[2] != 0
        weights = (1.0 / size(V)[2]) .* LA.diag(V * V')
        i = min(N - sum(rand(rng) .< cumsum(weights)) + 1, N)
        Y = union(Y, Set(i))
        V = orthogonal_subspace_basis(V, LA.I[1:N,i], tol)
    end
    return Y
end

end
