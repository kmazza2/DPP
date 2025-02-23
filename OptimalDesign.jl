module OptimalDesign

export orthogonal_subspace_basis

import LinearAlgebra as LA

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

end
