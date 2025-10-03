data {
  int<lower=1> N;         // Number of observations
  vector[N] x;            // Scaled time
  vector[N] y;            // Simulated prices
}
parameters {
  real beta0;             // Intercept (drift)
  real beta1;             // Slope (drift)
  real<lower=0> alpha;    // GP magnitude
  real<lower=0> rho;      // GP lengthscale
  real<lower=0> sigma;    // Observation noise
  vector[N] z;            // For non-centered GP
}
transformed parameters {
  vector[N] mu = beta0 + beta1 * x;    // Drift mean
  matrix[N, N] K;
  for (i in 1:N)
    for (j in 1:N)
      K[i, j] = alpha^2 * exp(-0.5 * square((x[i] - x[j]) / rho));
  for (n in 1:N)
    K[n, n] = K[n, n] + sigma^2;      // Add noise to the diagonal
  vector[N] f = mu + cholesky_decompose(K) * z;
}
model {
  // Priors (use weakly informative)
  beta0 ~ normal(0, 100);
  beta1 ~ normal(0, 100);
  alpha ~ normal(0, 10);
  rho ~ normal(0, 10);
  sigma ~ normal(0, 10);
  z ~ normal(0, 1);
  y ~ normal(f, sigma);
}





