data {
  int<lower=1> N;
  vector[N] x;
  vector[N] y;
}

parameters {
  real alpha;           // log(signal variance)
  real rho;             // log(length scale)
  real<lower=0> sigma;  // noise
}

model {
  matrix[N, N] K;
  vector[N] mu;

  // Priors (weakly informative)
  alpha ~ normal(0, 1);
  rho ~ normal(0, 1);
  sigma ~ normal(0, 1);

  // Kernel matrix
  for (i in 1:N) {
    for (j in i:N) {
      real sq_dist = square(x[i] - x[j]);
      K[i, j] = exp(2 * alpha) * exp(-0.5 * sq_dist / exp(2 * rho));
      if (i != j)
        K[j, i] = K[i, j];
    }
    K[i, i] = K[i, i] + square(sigma);  // Add noise term to diagonal
  }

  mu = rep_vector(0, N);
  y ~ multi_normal(mu, K);
}







