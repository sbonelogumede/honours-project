// file: gp_bayes_noncentered.stan
data {
  int<lower=1> N;                 // number of observed points
  array[N] real x;                    // inputs
  vector[N] y;                    // outputs
}
parameters {
  real<lower=0> alpha;            // GP marginal sd
  real<lower=0> rho;              // GP length-scale
  real<lower=0> sigma;            // noise sd
  vector[N] z;                    // non-centered latent: z ~ N(0, I)
}
transformed parameters {
  matrix[N,N] K = cov_exp_quad(x, alpha, rho)
                + diag_matrix(rep_vector(1e-10, N)); // jitter
  matrix[N,N] L_K = cholesky_decompose(K);
  vector[N] f = L_K * z;          // centered latent function values
}
model {
  // Priors (weakly-informative; adjust as needed)
  alpha ~ normal(0, 2);           // half-normal via <lower=0>
  rho   ~ inv_gamma(4.6, 22.1);   // length-scale prior
  sigma ~ normal(0, 1);

  // Non-centered latent
  z ~ normal(0, 1);

  // Likelihood
  y ~ normal(f, sigma);
}
generated quantities {
  // Posterior predictive at observed x
  vector[N] y_rep;
  for (n in 1:N) {
    y_rep[n] = normal_rng(f[n], sigma);
  }
}




