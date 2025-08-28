data {
  int<lower=1> N1;           // number observed
  array[N1] real x1;         // inputs
  vector[N1] y1;             // outputs
}

parameters {
  real<lower=0> rho;         // lengthscale
  real<lower=0> alpha;       // marginal deviation
  real<lower=0> sigma;       // obs noise
}

model {
  // covariance matrix with jitter and noise variance
  matrix[N1, N1] K = gp_exp_quad_cov(x1, alpha, rho)
                   + diag_matrix(rep_vector(square(sigma) + 1e-10, N1));
  matrix[N1, N1] L_K = cholesky_decompose(K);

  // priors
  rho   ~ normal(0, 3);
  alpha ~ normal(0, 1);
  sigma ~ normal(0, 1);

  // marginal likelihood
  y1 ~ multi_normal_cholesky(rep_vector(0, N1), L_K);
}






