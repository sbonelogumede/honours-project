data {
  int<lower=1> N;            // number of observations
  int<lower=1> N_pred;       // number of prediction points
  vector[N] x;               // training inputs
  vector[N] y;               // training outputs
  vector[N_pred] x_pred;     // prediction inputs
}
parameters {
  real<lower=0> alpha;       // signal variance
  real<lower=0> rho;         // length scale
  real<lower=0> sigma;       // noise
}
model {
  matrix[N, N] K;

  // build covariance for training
  for (i in 1:N) {
    for (j in 1:N) {
      K[i, j] = alpha^2 * exp(-0.5 * square((x[i] - x[j]) / rho));
    }
    K[i,i] += sigma^2;
  }

  y ~ multi_normal(rep_vector(0, N), K);
}
generated quantities {
  vector[N2] f_star;
  {
    // cross covariances
    matrix[N2, N1] K_star = gp_exp_quad_cov(x2, x1, alpha, rho);
    matrix[N1, N1] K = gp_exp_quad_cov(x1, alpha, rho) +
                       diag_matrix(rep_vector(square(sigma) + 1e-6, N1));
    matrix[N1, N1] L_K = cholesky_decompose(K);

    // Solve L_K * v = y1  (forward substitution)
    vector[N1] v = mdivide_left_tri_low(L_K, y1);

    // Solve L_K' * w = v  (backward substitution)
    vector[N1] w = mdivide_right_tri_low(L_K, v);

    // Posterior predictive mean
    f_star = K_star * w;
  }
}

