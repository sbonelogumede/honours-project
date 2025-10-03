data {
  int<lower=1> N;           // Number of observations
    real x[N];              // Inputs (scaled time)
  vector[N] y;              // Outputs (scaled price)
}
parameters {
  real<lower=0> alpha;      // Amplitude (signal std)
  real<lower=0> rho;        // Length-scale
  real<lower=0> sigma;      // Noise std
  vector[N] f;              // Latent function values
}
model {
  matrix[N, N] K = cov_exp_quad(x, alpha, rho);
  for (n in 1:N) K[n, n] = K[n, n] + square(sigma);  // Add noise
  f ~ multi_normal(rep_vector(0, N), K);             // GP prior
  y ~ normal(f, sigma);                              // Likelihood
}
generated quantities {
  vector[N] y_rep;
  for (n in 1:N)
    y_rep[n] = normal_rng(f[n], sigma);  // Posterior predictive for each point
}


