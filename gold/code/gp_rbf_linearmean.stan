data {
  int<lower=1> N;
  vector[N] x;
  vector[N] y;
}
parameters {
  real beta0;
  real beta1;
  real<lower=0> alpha;
  real<lower=0> rho;
  real<lower=0> sigma;
}
model {
  matrix[N, N] K;
  vector[N] mu;

  mu = beta0 + beta1 * x;

  for (i in 1:N) {
    for (j in i:N) {
      real sqdist = square((x[i] - x[j]) / rho);
      K[i, j] = square(alpha) * exp(-0.5 * sqdist);
      if (i == j)
        K[i, j] += square(sigma) + 1e-6;  // add jitter
      K[j, i] = K[i, j];
    }
  }

  beta0 ~ normal(0, 1);
  beta1 ~ normal(0, 1);
  alpha ~ normal(0, 1);
  rho   ~ normal(0, 1);
  sigma ~ exponential(1);

  y ~ multi_normal(mu, K);
}
