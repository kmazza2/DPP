module OptimalDesign

export orthogonal_subspace_basis, sample_L_ens, weighted_sample, bernoulli_trial

import LinearAlgebra as LA

"Returns orthonormal basis for subspace of column space of A orthogonal to u."
function orthogonal_subspace_basis(A, u, tol)
    # use SVD to find orthnormal basis for nullspace of u' * A
    product_decomp = LA.svd(u' * A, full=true)
    # columns of V corresponding to zeros on diagonal of S
    # form orthonormal basis for nullspace of matrix
    # (Trefethen p. 33, p. 36)
    nullspace_basis = product_decomp.V[
        :,
        [
            abs.(product_decomp.S);
            zeros(size(A,2) - length(product_decomp.S))
        ] .< tol
    ]
    # columns of A * nullspace_basis are spanning set for subspace of
    # column space of A orthogonal to u
    #
    # use SVD to find orthonormal basis for column space of
    # A * nullspace_basis (since the column space is the
    # subspace of the column space of A orthogonal to u,
    # this completes the computation)
    spanning_set_decomp = LA.svd(A * nullspace_basis, full=true)
    # columns of U corresponding to nonzero values on diagonal
    # of S form orthonormal basis for column space of matrix
    # (Trefethen p. 33, p. 36)
    spanning_set_basis = spanning_set_decomp.U[
        :,
        [
            abs.(spanning_set_decomp.S);
            zeros(size(A,1) - length(spanning_set_decomp.S))
        ] .> tol
    ]
    return spanning_set_basis
end

function sample_L_ens(L, tol, rng)
    @assert LA.issymmetric(L)
    v, M = LA.eigen(L)
    @assert all(v .>= 0.0)
    N = size(L)[1]
    Y = Set()
    V = M[:, map(l -> bernoulli_trial(l / (l + 1.0), rng), v)]
    while size(V)[2] != 0
        weights = LA.diag(V * V')
        i = weighted_sample(weights, rng)
        Y = union(Y, Set(i))
        V = orthogonal_subspace_basis(V, LA.I[1:N,i], tol)
    end
    return Y
end

function weighted_sample(weights, rng)
    N = length(weights)
    weights = weights ./ sum(weights)
    return min(N - sum(rand(rng) .< cumsum(weights)) + 1, N)
end

function bernoulli_trial(p, rng)
    return rand(rng) < p
end

end
