data {
  int<lower=1> n;
  real x[n];

  real<lower=0> rho;
  real<lower=0> alpha;
}

transformed data {
  matrix[n, n] cov =   cov_exp_quad(x, alpha, rho)
                     + diag_matrix(rep_vector(1e-10, n));
  matrix[n, n] L_cov = cholesky_decompose(cov);
}

parameters {}
model {}

generated quantities {
  vector[n] f = multi_normal_cholesky_rng(rep_vector(0, n), L_cov);
}
